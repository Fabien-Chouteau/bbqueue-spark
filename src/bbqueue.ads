with System;
with Interfaces;

--  private with Atomic.Unsigned_32;
private with Atomic.Generic32_SPARK;

package BBqueue
--  with Preelaborate
with SPARK_Mode
is
   use type Interfaces.Unsigned_32;

   type Result_Kind is (Success, Grant_In_Progress, Insufficient_Size, Empty);

   type Slice is limited private;

   subtype Count is Interfaces.Unsigned_32;

   subtype Buffer_Size is Count range 2 .. 55;

   type Buffer (Size : Buffer_Size) is limited private;

   -- Producer --

   procedure Grant (This : in out Buffer;
                    G    : in out Slice;
                    Size : Count);

   procedure Commit (This : in out Buffer;
                     Size :        Count := Count'Last);
   --  Size can be smaller than the granted slice for partial commits

   -- Consumer --

   procedure Read (This : in out Buffer;
                   G    : in out Slice);

   procedure Release (This : in out Buffer;
                      Size :        Count := Count'Last);

   function Empty_Slice return Slice
     with Post => State (Empty_Slice'Result) = Empty;

   --  Tmp Internals
   function State (G : Slice) return Result_Kind;
   function Size (G : Slice) return Count
     with Pre => State (G) = Success;
   function Index (G : Slice) return Count
     with Pre => State (G) = Success;
   function Addr (G : Slice) return System.Address
     with Pre => State (G) = Success;

   procedure Print (This : Buffer);

private

   --  package Atomic_Count renames Atomic.Unsigned_32;
   package Atomic_Count is new Atomic.Generic32_SPARK (Interfaces.Unsigned_32);
   use Atomic_Count;

   subtype Buffer_Index is Count range 1 .. Buffer_Size'Last;
   type Storage_Array is array (Buffer_Index range <>) of Interfaces.Unsigned_8;

   type Buffer (Size : Buffer_Size) is limited record
      Buf : Storage_Array (1 .. Size) := (others => 0);

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

   type Slice is limited record
      Result    : Result_Kind := Empty;
      Size      : Count := 0;
      Index     : Count := 0;
      Addr      : System.Address := System.Null_Address;
   end record;

   function State (G : Slice) return Result_Kind
   is (G.Result);
   function Empty_Slice return Slice
   is (Result => Empty, others => <>);
   function Size (G : Slice) return Count
   is (G.Size);
   function Index (G : Slice) return Count
   is (G.Index);
   function Addr (G : Slice) return System.Address
   is (G.Addr);

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
end BBqueue;
