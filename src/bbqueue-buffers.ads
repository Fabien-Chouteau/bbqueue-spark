--  This unit is based on BBQueue.Offsets_Only and embeds an internal buffer.
--  It provides directly usable slice of memory from its internal buffer:
--
--     Q   : aliased Buffer (64);
--     WG  : Write_Grant := Empty;
--     S   : Slice_Rec;
--  begin
--     Grant (Q, WG, 8);
--     if State (WG) = Valid then
--        declare
--           B : Storage_Array (1 .. Slice (WG).Length)
--             with Address => Slice (WG).Addr;
--        begin
--           B := (others => 42);
--        end;
--     end if;
--     Commit (Q, WG);

with System;

package BBqueue.Buffers
with Preelaborate,
     SPARK_Mode,
     Abstract_State => null
is

   type Buffer (Size   : Buffer_Size)
   is limited private;

   -- Producer --

   type Write_Grant is limited private;

   procedure Grant (This : in out Buffer;
                    G    : in out Write_Grant;
                    Size : Count)
     with Global => null,
          Pre  => State (G) /= Valid,
          Post => State (G) in Valid | Empty | Grant_In_Progress | Insufficient_Size
                  and then
                   (if Size = 0 then State (G) = Empty)
                  and then
                   (if State (G) = Valid
                     then Write_Grant_In_Progress (This)
                     and then Slice (G).Length = Size);
   --  Request a contiguous writeable slice of the internal buffer

   procedure Commit (This : in out Buffer;
                     G    : in out Write_Grant;
                     Size :        Count := Count'Last)
     with Pre  => State (G) = Valid,
          Post => (if Write_Grant_In_Progress (This)'Old
                   then State (G) = Empty
                   else State (G) = Valid);
   --  Commit a writeable slice. Size can be smaller than the granted slice for
   --  partial commits. The commited slice is then available for Read.

   generic
      with procedure Process_Write (Data : out Storage_Array; To_Commit : out Count);
   procedure Write_CB (This   : in out Buffer;
                       Size   :        Count;
                       Result :    out Result_Kind);
   --  Write in the buffer using a "callback". This procedure will call
   --  Process_Write () on the slice returned by Grant (), if the result
   --  is Valid. It will then call Commit with the value To_Commit returned by
   --  Process_Write ().

   -- Consumer --

   type Read_Grant is limited private;

   procedure Read (This : in out Buffer;
                   G    : in out Read_Grant;
                   Max  :        Count := Count'Last)
     with Pre  => State (G) /= Valid,
          Post => State (G) in Valid | Empty | Grant_In_Progress
                  and then
                  (if State (G) = Valid
                       then Read_Grant_In_Progress (This)
                   and then Slice (G).Length <= Max);
   --  Request contiguous readable slice of up to Max elements from the internal
   --  buffer.

   procedure Release (This : in out Buffer;
                      G    : in out Read_Grant;
                      Size :        Count := Count'Last)
     with Pre  => State (G) = Valid,
          Post => (if Read_Grant_In_Progress (This)'Old
                   then State (G) = Empty
                   else State (G) = Valid);
   --  Release a readable slice. Size can be smaller than the granted slice for
   --  partial releases.

   generic
      with procedure Process_Read (Data : Storage_Array; To_Release : out Count);
   procedure Read_CB (This   : in out Buffer;
                      Result :    out Result_Kind);
   --  Read from the buffer using a "callback". This procedure will call
   --  Process_Read () on the slice returned by Read (), if the result is
   --  Valid. It will then call Release with the value To_Release returned
   --  by Process_Read ().

   -- Utils --

   function Empty return Write_Grant
     with Post => State (Empty'Result) = Empty;
   function Empty return Read_Grant
     with Post => State (Empty'Result) = Empty;

   -- Slices --

   type Slice_Rec is record
      Length : Count;
      Addr   : System.Address;
   end record;

   function State (G : Write_Grant) return Result_Kind;
   function Slice (G : Write_Grant) return Slice_Rec
     with Pre => State (G) = Valid;

   function State (G : Read_Grant) return Result_Kind;
   function Slice (G : Read_Grant) return Slice_Rec
     with Pre => State (G) = Valid;

   function Write_Grant_In_Progress (This : Buffer) return Boolean
     with Ghost;
   function Read_Grant_In_Progress (This : Buffer) return Boolean
     with Ghost;

private

   type Buffer (Size  : Buffer_Size) is limited record
      Buf     : Storage_Array (1 .. Size) := (others => 0);
      Offsets : Offsets_Only (Size);
   end record;

   function Empty_Slicerec return Slice_Rec
   is (0, System.Null_Address);

   type Write_Grant is limited record
      Offsets_Grant : BBqueue.Write_Grant;
      Slice  : Slice_Rec := (0, System.Null_Address);
   end record;

   type Read_Grant is limited record
      Offsets_Grant : BBqueue.Read_Grant;
      Slice  : Slice_Rec := (0, System.Null_Address);
   end record;

   function State (G : Write_Grant) return Result_Kind
   is (G.Offsets_Grant.Result);
   function Empty return Write_Grant
   is (Offsets_Grant => BBqueue.Empty, others => <>);
   function Slice (G : Write_Grant) return Slice_Rec
   is (G.Slice);

   function State (G : Read_Grant) return Result_Kind
   is (G.Offsets_Grant.Result);
   function Empty return Read_Grant
   is (Offsets_Grant => BBqueue.Empty, others => <>);
   function Slice (G : Read_Grant) return Slice_Rec
   is (G.Slice);

   -----------------------------
   -- Write_Grant_In_Progress --
   -----------------------------

   function Write_Grant_In_Progress (This : Buffer) return Boolean
   is (BBqueue.Write_Grant_In_Progress (This.Offsets));

   ----------------------------
   -- Read_Grant_In_Progress --
   ----------------------------

   function Read_Grant_In_Progress (This : Buffer) return Boolean
   is (BBqueue.Read_Grant_In_Progress (This.Offsets));

end BBqueue.Buffers;
