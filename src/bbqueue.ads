with Interfaces;

with Array_Slices;

private with Atomic.Generic32;

package BBqueue
with Preelaborate,
     SPARK_Mode
is
   use type Interfaces.Unsigned_32;

   type Result_Kind is (Valid, Grant_In_Progress, Insufficient_Size, Empty);

   subtype Count is Interfaces.Unsigned_32;

   subtype Buffer_Size is Count range 1 .. Count'Last / 2;
   --  To avoid interger overflow the buffer size is limited to (2^32)/2.
   --  That is a buffer of 2 Gigabytes.

   type Buffer (Size : Buffer_Size) is limited private;

   -- Producer --

   type Write_Grant is limited private;

   procedure Grant (This : in out Buffer;
                    G    : in out Write_Grant;
                    Size : Count)
     with Pre  => State (G) /= Valid,
          Post => (if State (G) = Valid then Write_Grant_In_Progress (This));

   procedure Commit (This : in out Buffer;
                     G    : in out Write_Grant;
                     Size :        Count := Count'Last)
     with Pre  => State (G) = Valid,
          Post => (if Write_Grant_In_Progress (This)'Old
                   then State (G) = Empty
                   else State (G) = Valid);
   --  Size can be smaller than the granted slice for partial commits

   -- Consumer --

   type Read_Grant is limited private;

   procedure Read (This : in out Buffer;
                   G    : in out Read_Grant)
     with Pre  => State (G) /= Valid,
          Post => State (G) in Valid | Empty | Grant_In_Progress
                  and then
                  (if State (G) = Valid then Read_Grant_In_Progress (This));

   procedure Release (This : in out Buffer;
                      G    : in out Read_Grant;
                      Size :        Count := Count'Last)
     with Pre  => State (G) = Valid,
          Post => (if Read_Grant_In_Progress (This)'Old
                   then State (G) = Empty
                   else State (G) = Valid);

   -- Utils --

   function Empty return Write_Grant
     with Post => State (Empty'Result) = Empty;
   function Empty return Read_Grant
     with Post => State (Empty'Result) = Empty;

   -- Slices --

   subtype Buffer_Index is Count range 1 .. Buffer_Size'Last;
   type Storage_Array is array (Buffer_Index range <>) of Interfaces.Unsigned_8;

   package Slices is new Array_Slices (Count,
                                       Buffer_Index,
                                       Interfaces.Unsigned_8,
                                       Storage_Array);

   function State (G : Write_Grant) return Result_Kind;
   function Slice (G : Write_Grant) return Slices.Slice
     with Pre => State (G) = Valid;

   function State (G : Read_Grant) return Result_Kind;
   function Slice (G : Read_Grant) return Slices.Slice
     with Pre => State (G) = Valid;

   --  Contract helpers --

   function Write_Grant_In_Progress (This : Buffer) return Boolean with Ghost;
   function Read_Grant_In_Progress (This : Buffer) return Boolean with Ghost;

private

   package Atomic_Count is new Atomic.Generic32 (Count);
   use Atomic_Count;

   type Buffer (Size : Buffer_Size) is limited record
      Buf : aliased Storage_Array (1 .. Size) := (others => 0);

      Write : aliased Atomic_Count.Instance := Atomic_Count.Init (0);
      --  Where the next byte will be written

      Read  : aliased Atomic_Count.Instance := Atomic_Count.Init (0);
      --  Where the next byte will be read from

      Last  : aliased Atomic_Count.Instance := Atomic_Count.Init (0);
      --  Used in the inverted case to mark the end of the
      --  readable streak. Otherwise will == sizeof::<self.buf>().
      --  Writer is responsible for placing this at the correct
      --  place when entering an inverted condition, and Reader
      --  is responsible for moving it back to sizeof::<self.buf>()
      --  when exiting the inverted condition
      --
      --  Cooperatively owned
      --
      --  NOTE: This should generally be initialized as size_of::<self.buf>(),
      --  however this would prevent the structure from being entirely
      --  zero-initialized, and can cause the .data section to be much larger
      --  than necessary. By forcing the `last` pointer to be zero initially, we
      --  place the structure in an "inverted" condition, which will be resolved
      --  on the first commited bytes that are written to the structure.
      --
      --  When read == last == write, no bytes will be allowed to be read
      --  (good), but write grants can be given out (also good).

      Reserve  : aliased Atomic_Count.Instance := Atomic_Count.Init (0);
      --  Used by the Writer to remember what bytes are currently
      --  allowed to be written to, but are not yet ready to be
      --  read from

      Read_In_Progress : aliased Atomic.Flag := Atomic.Init (False);
      --  Is there an active read grant?

      Write_In_Progress : aliased Atomic.Flag := Atomic.Init (False);
      --  Is there an active write grant?

      Granted_Write_Size : Count := 0;
      Granted_Read_Size : Count := 0;
   end record
     with Invariant =>
              Buf'Last <= Buffer_Size'Last
     and then Value (Write) in 0 .. Buf'Last
     and then Value (Read) in 0 .. Buf'Last
     and then Value (Last) in 0 .. Buf'Last
     and then Value (Reserve) in 0 .. Buf'Last
     and then Value (Last) >= Value (Read)
     and then Value (Last) >= Value (Write)
     and then Granted_Write_Size <= Buffer_Size'Last
     and then Granted_Read_Size <= Buffer_Size'Last

     --  Reserve can only be lower than Write when a write grant made an
     --  inverted allocation (starting back at 0), but the grant is not
     --  commited yet.
     and then (if Value (Reserve) < Value (Write) then not Is_Inverted (Buffer))
     and then (if Value (Reserve) < Value (Write)
               then not Is_Inverted (Buffer)
                    and then Value (Write) >= Value (Read)
                    and then Value (Reserve) = Granted_Write_Size
               else Value (Reserve) = Value (Write) + Granted_Write_Size)

     --  Reserve is always in a writable area or else = Buf'Last
     and then (In_Writable_Area (Buffer, Value (Reserve))
               or else Value (Reserve) = Buf'Last)

     and then (if Is_Inverted (Buffer)
               then (Value (Write) + Granted_Write_Size <= Value (Read)
                     and then
                     Value (Reserve) <= Value (Read))
               else Value (Read) + Granted_Read_Size <= Value (Write))

     --  Read cannot be in reserved area
     and then (not In_Reserved_Area (Buffer, Value (Read))
               or else Value (Read) = Value (Write))

     --  Write grant bounds
     and then (if Is_Inverted (Buffer)
               then Granted_Write_Size <= Value (Read) - Value (Write)
               else Granted_Write_Size <= Count'Max (Buf'Last - Value (Write),
                                                         Value (Read)))
     --  Read grant bounds
     and then (if Is_Inverted (Buffer)
               then Granted_Read_Size <= Value (Last) - Value (Read)
               else Granted_Read_Size <= Value (Write) - Value (Read))

     --  Reserve when about to invert
     and then (if not Is_Inverted (Buffer) and then Value (Reserve) < Value (Write) then
                 --  When Reserved wrapped around, we know that it is because we
                 --  needed more space than what is available between Write and
                 --  then end of the buffer
                 Value (Reserve) > (Buf'Last - Value (Write))
              )

     and then (Value (Read) + Granted_Read_Size in 0 .. Buf'Last)
     and then (if not Atomic.Value (Write_In_Progress) then Granted_Write_Size = 0)
     and then (if not Atomic.Value (Read_In_Progress) then Granted_Read_Size = 0)
     --  and then (if Atomic.Value (Write_In_Progress) then Granted_Write_Size /= 0)
     --  and then (if Atomic.Value (Read_In_Progress) then Granted_Read_Size /= 0)
   ;

   function Is_Inverted (This : Buffer) return Boolean
   is (Value (This.Write) < Value (This.Read))
     with Ghost;

   type Write_Grant is limited record
      Result : Result_Kind := Empty;
      Slice  : Slices.Slice := Slices.Empty_Slice;
   end record;
     --  with Invariant => (case Write_Grant.Result is
     --                       when Valid => not Slices.Empty (Write_Grant.Slice),
     --                       when others => Slices.Empty (Write_Grant.Slice));

   type Read_Grant is limited record
      Result : Result_Kind := Empty;
      Slice  : Slices.Slice := Slices.Empty_Slice;
   end record;
     --  with Invariant => (case Read_Grant.Result is
     --                       when Valid => not Slices.Empty (Read_Grant.Slice),
     --                       when others => Slices.Empty (Read_Grant.Slice));

   function State (G : Write_Grant) return Result_Kind
   is (G.Result);
   function Empty return Write_Grant
   is (Result => Empty, others => <>);
   function Slice (G : Write_Grant) return Slices.Slice
   is (G.Slice);

   function State (G : Read_Grant) return Result_Kind
   is (G.Result);
   function Empty return Read_Grant
   is (Result => Empty, others => <>);
   function Slice (G : Read_Grant) return Slices.Slice
   is (G.Slice);

   function Get_Slice (This : Buffer;
                       From : Buffer_Index;
                       Size : Count)
                       return Slices.Slice
     with Pre  => Size /= 0
                  and then From in This.Buf'Range
                  and then From + Size - 1 in This.Buf'Range,
          Post => not Slices.Empty (Get_Slice'Result);

   --  Contract helpers --

   ----------------------
   -- In_Readable_Area --
   ----------------------

   function In_Readable_Area (This : Buffer; Index : Count) return Boolean
   is (if Is_Inverted (This) then
         --  Already inverted.
         (if Value (This.Read) /= Value (This.Last) then
             --  |===W-----------R==L..|
             --  Data remaining before Last:
             --  We can read between R .. L
             Index in Value (This.Read) .. Value (This.Last)
          else
             --  |===W--------------R..|
             --                     L
             --  Read = Last, the next valid read is inverted:
             --  We can read between 0 .. W - 1
             Index in 0 .. Value (This.Write) - 1)
       else
          --  |----R=========W-----|
          --  Not Inverted (R <= W):
          --  We can read between R .. W - 1
          Index in Value (This.Read) .. Value (This.Write) - 1)
     with Ghost;

   ----------------------
   -- In_Writable_Area --
   ----------------------

   function In_Writable_Area (This : Buffer; Index : Count) return Boolean
   is (if Is_Inverted (This) then
          --  Already inverted
          --  |---W==========R----|
          --  Inverted (R > W):
          --  We can write between W .. R - 1
          Index in Value (This.Write) .. Value (This.Read) - 1
       else (
             --  |====R---------W=====|
             --  Not Inverted (R <= W):
             --  We can write between W .. Last - 1, or 0 .. R - 1 if we invert
               (Index in Value (This.Write) .. This.Buf'Last - 1)
             or else
               (Index in 0 .. Value (This.Read) - 1)))
       with Ghost;

   ----------------------
   -- In_Reserved_Area --
   ----------------------

   function In_Reserved_Area (This : Buffer; Index : Count) return Boolean
   is (This.Granted_Write_Size /= 0
       and then
       Index in Value (This.Reserve) - This.Granted_Write_Size .. Value (This.Reserve) - 1
      )
   with Ghost;

   -----------------------------
   -- Write_Grant_In_Progress --
   -----------------------------

   function Write_Grant_In_Progress (This : Buffer) return Boolean
   is (Atomic.Value (This.Write_In_Progress));

   ----------------------------
   -- Read_Grant_In_Progress --
   ----------------------------

   function Read_Grant_In_Progress (This : Buffer) return Boolean
   is (Atomic.Value (This.Read_In_Progress));

end BBqueue;
