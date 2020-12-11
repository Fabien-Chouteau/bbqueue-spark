with System.Storage_Elements; use System.Storage_Elements;
private with Atomic.Generic_Signed64;

--  This unit only works with buffer offset without having an internal buffer.
--  It can be used to allocate slices of an existing array, e.g.:

--     Buf : Storage_Array (8 .. 64) := (others => 0);
--     Q   : aliased Offsets_Only (Buf'Length);
--     WG  : Write_Grant := BBqueue.Empty;
--     S   : Slice_Rec;
--  begin
--     Write_Grant (Q, WG, 8);
--     if State (WG) = Valid then
--        S := Slice (WG);
--        Buf (Buf'First + S.Start_Offset .. Buf'First + S.End_Offset) := (others => 42);
--     end if;
--     Commit (Q, WG);

package BBqueue
with Preelaborate,
     SPARK_Mode
is
   type Result_Kind is (Valid, Grant_In_Progress, Insufficient_Size, Empty);

   subtype Count is Storage_Count;

   subtype Buffer_Size is Count range 1 .. Count'Last / 2;
   --  To avoid interger overflow the buffer size is limited to (2^32)/2.
   --  That is a buffer of 2 Gigabytes.

   subtype Buffer_Offset is Storage_Offset range 0 .. Count'Last - 1;

   type Offsets_Only (Size : Buffer_Size) is limited private;

   type Slice_Rec is record
      Length       : Count;
      Start_Offset : Buffer_Offset;
      End_Offset   : Buffer_Offset;
   end record;

   -- Producer --

   type Write_Grant is limited private;

   procedure Grant (This : in out Offsets_Only;
                    G    : in out Write_Grant;
                    Size : Count)
     with Pre  => State (G) /= Valid,
          Post => State (G) in Valid | Empty | Grant_In_Progress | Insufficient_Size
                  and then
                  (if State (G) = Valid
                       then Write_Grant_In_Progress (This)
                   and then Valid_Slice (This, Slice (G))
                   and then Valid_Write_Slice (This, Slice (G)));

   procedure Commit (This : in out Offsets_Only;
                     G    : in out Write_Grant;
                     Size :        Count := Count'Last)
     with Pre  => State (G) = Valid,
          Post => (if Write_Grant_In_Progress (This)'Old
                   then State (G) = Empty
                   else State (G) = Valid);
   --  Size can be smaller than the granted slice for partial commits

   -- Consumer --

   type Read_Grant is limited private;

   procedure Read (This : in out Offsets_Only;
                   G    : in out Read_Grant)
     with Pre  => State (G) /= Valid,
          Post => State (G) in Valid | Empty | Grant_In_Progress
                  and then
                  (if State (G) = Valid
                       then Read_Grant_In_Progress (This)
                   and then Valid_Slice (This, Slice (G))
                   and then Valid_Read_Slice (This, Slice (G)));

   procedure Release (This : in out Offsets_Only;
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

   function State (G : Write_Grant) return Result_Kind;
   function Slice (G : Write_Grant) return Slice_Rec
     with Pre => State (G) = Valid;

   function State (G : Read_Grant) return Result_Kind;
   function Slice (G : Read_Grant) return Slice_Rec
     with Pre => State (G) = Valid;

   --  Contract helpers --

   function Valid_Slice (This : Offsets_Only; Slice : Slice_Rec) return Boolean
   is (Slice.Start_Offset <= Slice.End_Offset
       and then Slice.Length = Slice.End_Offset - Slice.Start_Offset + 1
       and then Slice.Start_Offset in 0 .. This.Size - 1
       and then Slice.End_Offset in 0 .. This.Size - 1)
     with Ghost;
   --  A valid slice contains offsets within the bounds of the array range.
   --  This ensures that:
   --  Arr (Arr'First + Start_Offset .. Arr'First + End_Offset)
   --  will never be out of bounds.

   function Valid_Write_Slice (This : Offsets_Only; Slice : Slice_Rec) return Boolean
     with Ghost;

   function Valid_Read_Slice (This : Offsets_Only; Slice : Slice_Rec) return Boolean
     with Ghost;

   function Write_Grant_In_Progress (This : Offsets_Only) return Boolean with Ghost;
   function Read_Grant_In_Progress (This : Offsets_Only) return Boolean with Ghost;

private

   function In_Readable_Area (This   : Offsets_Only;
                              Offset : Buffer_Offset)
                              return Boolean
     with Ghost;

   function In_Writable_Area (This   : Offsets_Only;
                              Offset : Buffer_Offset)
                              return Boolean
     with Ghost;
   function In_Reserved_Area (This : Offsets_Only;
                              Offset : Buffer_Offset)
                              return Boolean
     with Ghost;

   package Atomic_Count is new Atomic.Generic_Signed64 (Count);
   use Atomic_Count;

   type Offsets_Only (Size : Buffer_Size) is limited record
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
              Size <= Buffer_Size'Last
     and then Value (Write) in 0 .. Size
     and then Value (Read) in 0 .. Size
     and then Value (Last) in 0 .. Size
     and then Value (Reserve) in 0 .. Size
     and then Value (Last) >= Value (Read)
     and then Value (Last) >= Value (Write)
     and then Granted_Write_Size <= Buffer_Size'Last
     and then Granted_Read_Size <= Buffer_Size'Last

     --  Reserve can only be lower than Write when a write grant made an
     --  inverted allocation (starting back at 0), but the grant is not
     --  commited yet.
     and then (if Value (Reserve) < Value (Write) then not Is_Inverted (Offsets_Only))
     and then (if Value (Reserve) < Value (Write)
               then not Is_Inverted (Offsets_Only)
                    and then Value (Write) >= Value (Read)
                    and then Value (Reserve) = Granted_Write_Size
               else Value (Reserve) = Value (Write) + Granted_Write_Size)

     --  Reserve is always in a writable area or else = Size
     and then (In_Writable_Area (Offsets_Only, Value (Reserve))
               or else Value (Reserve) = Size)

     and then (if Is_Inverted (Offsets_Only)
               then (Value (Write) + Granted_Write_Size <= Value (Read)
                     and then
                     Value (Reserve) <= Value (Read))
               else Value (Read) + Granted_Read_Size <= Value (Write))

     --  Read cannot be in reserved area
     and then (not In_Reserved_Area (Offsets_Only, Value (Read))
               or else Value (Read) = Value (Write))

     --  Write grant bounds
     and then (if Is_Inverted (Offsets_Only)
               then Granted_Write_Size <= Value (Read) - Value (Write)
               else Granted_Write_Size <= Count'Max (Size - Value (Write),
                                                         Value (Read)))
     --  Read grant bounds
     and then (if Is_Inverted (Offsets_Only)
               then Granted_Read_Size <= Value (Last) - Value (Read)
               else Granted_Read_Size <= Value (Write) - Value (Read))

     --  Reserve when about to invert
     and then (if not Is_Inverted (Offsets_Only) and then Value (Reserve) < Value (Write) then
                 --  When Reserved wrapped around, we know that it is because we
                 --  needed more space than what is available between Write and
                 --  then end of the buffer
                 Value (Reserve) > (Size - Value (Write))
              )

     and then (Value (Read) + Granted_Read_Size in 0 .. Size)
     and then (if not Atomic.Value (Write_In_Progress) then Granted_Write_Size = 0)
     and then (if not Atomic.Value (Read_In_Progress) then Granted_Read_Size = 0)
     --  and then (if Atomic.Value (Write_In_Progress) then Granted_Write_Size /= 0)
     --  and then (if Atomic.Value (Read_In_Progress) then Granted_Read_Size /= 0)
   ;

   function Is_Inverted (This : Offsets_Only) return Boolean
   is (Value (This.Write) < Value (This.Read))
     with Ghost;

   type Write_Grant is limited record
      Result : Result_Kind := Empty;
      Slice  : Slice_Rec := (0, 0, 0);
   end record;
     --  with Invariant => (case Write_Grant.Result is
     --                       when Valid => not Slices.Empty (Write_Grant.Slice),
     --                       when others => Slices.Empty (Write_Grant.Slice));

   type Read_Grant is limited record
      Result : Result_Kind := Empty;
      Slice  : Slice_Rec := (0, 0, 0);
   end record;
     --  with Invariant => (case Read_Grant.Result is
     --                       when Valid => not Slices.Empty (Read_Grant.Slice),
     --                       when others => Slices.Empty (Read_Grant.Slice));

   function State (G : Write_Grant) return Result_Kind
   is (G.Result);
   function Empty return Write_Grant
   is (Result => Empty, others => <>);
   function Slice (G : Write_Grant) return Slice_Rec
   is (G.Slice);

   function State (G : Read_Grant) return Result_Kind
   is (G.Result);
   function Empty return Read_Grant
   is (Result => Empty, others => <>);
   function Slice (G : Read_Grant) return Slice_Rec
   is (G.Slice);

   Empty_Slice : constant Slice_Rec := (0, 0, 0);

   --  Contract helpers --

   ----------------------
   -- In_Readable_Area --
   ----------------------

   function In_Readable_Area (This : Offsets_Only; Offset : Buffer_Offset) return Boolean
   is (if Is_Inverted (This) then
         --  Already inverted.
         (if Value (This.Read) /= Value (This.Last) then
             --  |===W-----------R==L..|
             --  Data remaining before Last:
             --  We can read between R .. L
             Offset in Value (This.Read) .. Value (This.Last)
          else
             --  |===W--------------R..|
             --                     L
             --  Read = Last, the next valid read is inverted:
             --  We can read between 0 .. W - 1
             Offset in 0 .. Value (This.Write) - 1)
       else
          --  |----R=========W-----|
          --  Not Inverted (R <= W):
          --  We can read between R .. W - 1
          Offset in Value (This.Read) .. Value (This.Write) - 1);

   ----------------------
   -- In_Writable_Area --
   ----------------------

   function In_Writable_Area (This : Offsets_Only; Offset : Buffer_Offset) return Boolean
   is (if Is_Inverted (This) then
          --  Already inverted
          --  |---W==========R----|
          --  Inverted (R > W):
          --  We can write between W .. R - 1
          Offset in Value (This.Write) .. Value (This.Read) - 1
       else (
             --  |====R---------W=====|
             --  Not Inverted (R <= W):
             --  We can write between W .. Last - 1, or 0 .. R - 1 if we invert
               (Offset in Value (This.Write) .. This.Size - 1)
             or else
               (Offset in 0 .. Value (This.Read) - 1)));

   ----------------------
   -- In_Reserved_Area --
   ----------------------

   function In_Reserved_Area (This : Offsets_Only; Offset : Buffer_Offset) return Boolean
   is (This.Granted_Write_Size /= 0
       and then
       Offset in Value (This.Reserve) - This.Granted_Write_Size .. Value (This.Reserve) - 1
      );

   -----------------------
   -- Valid_Write_Slice --
   -----------------------

   function Valid_Write_Slice (This : Offsets_Only; Slice : Slice_Rec) return Boolean
   is (Valid_Slice (This, Slice)
       and then In_Writable_Area (This, Slice.Start_Offset)
       and then In_Writable_Area (This, Slice.End_Offset));

   function Valid_Read_Slice (This : Offsets_Only; Slice : Slice_Rec) return Boolean
   is (Valid_Slice (This, Slice)
       and then In_Readable_Area (This, Slice.Start_Offset)
       and then In_Readable_Area (This, Slice.End_Offset));

   -----------------------------
   -- Write_Grant_In_Progress --
   -----------------------------

   function Write_Grant_In_Progress (This : Offsets_Only) return Boolean
   is (Atomic.Value (This.Write_In_Progress));

   ----------------------------
   -- Read_Grant_In_Progress --
   ----------------------------

   function Read_Grant_In_Progress (This : Offsets_Only) return Boolean
   is (Atomic.Value (This.Read_In_Progress));

end BBqueue;
