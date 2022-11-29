--  This unit is based on BBqueue.Buffers and uses markers in the buffer to
--  track the size of each commited write grants. The size of consequent read
--  grants will conrespond to the sizes of commited write grants.
--
--  It can be used to handle variable lenght packets:
--
--     Q   : aliased Framed_Buffer (64);
--     WG  : Write_Grant := Empty;
--     RG  : Read_Grant := Empty;
--     S   : Slice_Rec;
--  begin
--     Grant (Q, WG, 8); -- Get a worst case grant of size 8
--     Commit (Q, WG, 4); -- Only commit 4
--     Grant (Q, WG, 8); -- Get a worst case grant of size 8
--     Commit (Q, WG, 5); -- Only commit 5
--     Read (W, RG); -- Returns a grant of size 4

with System;

package BBqueue.Buffers.framed
with Preelaborate,
     SPARK_Mode,
     Abstract_State => null
is

   Max_Frame_Header_Size : constant := 9;

   subtype Framed_Count
     is Count range Count'First .. Count'Last - Max_Frame_Header_Size;
   --  The frame can take up to 9 bytes in addition to the allocated size. The
   --  size of what can be allocated is therefore lower than for a non-framed
   --  buffer.

   type Framed_Buffer (Size   : Buffer_Size)
   is limited private;

   -- Producer --

   type Write_Grant is limited private;

   procedure Grant (This : in out Framed_Buffer;
                    G    : in out Write_Grant;
                    Size : Framed_Count)
     with Pre  => State (G) /= Valid,
          Post => State (G) in Valid | Empty | Grant_In_Progress |
                          Insufficient_Size
                  and then
                  (if State (G) = Valid then Write_Grant_In_Progress (This));
   --  Request a contiguous writeable slice of the internal buffer

   procedure Commit (This : in out Framed_Buffer;
                     G    : in out Write_Grant;
                     Size :        Framed_Count := Framed_Count'Last)
     with Pre  => State (G) = Valid,
          Post => (if Write_Grant_In_Progress (This)'Old
                   then State (G) = Empty
                   else State (G) = Valid);
   --  Commit a writeable slice. Size can be smaller than the granted slice for
   --  partial commits. The commited slice is then available for Read.

   generic
      with procedure Process_Write (Data      : out Storage_Array;
                                    To_Commit : out Count);
   procedure Write_CB (This   : in out Framed_Buffer;
                       Size   :        Framed_Count;
                       Result :    out Result_Kind);
   --  Write in the buffer using a "callback". This procedure will call
   --  Process_Write () on the slice returned by Grant (), if the result
   --  is Valid. It will then call Commit with the value To_Commit returned by
   --  Process_Write ().

   -- Consumer --

   type Read_Grant is limited private;

   procedure Read (This : in out Framed_Buffer;
                   G    : in out Read_Grant)
     with Pre  => State (G) /= Valid,
          Post => State (G) in Valid | Empty | Grant_In_Progress
                  and then
                  (if State (G) = Valid then Read_Grant_In_Progress (This));
   --  Request contiguous readable slice from the internal buffer. The size of
   --  the returned Read_Grant will be based on the size of previously commited
   --  frames.

   procedure Release (This : in out Framed_Buffer;
                      G    : in out Read_Grant)
     with Pre  => State (G) = Valid,
          Post => (if Read_Grant_In_Progress (This)'Old
                   then State (G) = Empty
                   else State (G) = Valid);
   --  Release a readable slice. Partial releases not allowed, the full grant
   --  will be released.

   generic
      with procedure Process_Read (Data : Storage_Array);
   procedure Read_CB (This   : in out Framed_Buffer;
                      Result :    out Result_Kind);
   --  Read from the buffer using a "callback". This procedure will call
   --  Process_Read () on the slice returned by Read (), if the result is
   --  Valid.

   -- Utils --

   function Empty return Write_Grant
     with Post => State (Empty'Result) = Empty;
   function Empty return Read_Grant
     with Post => State (Empty'Result) = Empty;

   function State (G : Write_Grant) return Result_Kind;
   function Slice (G : Write_Grant) return Slice_Rec
     with Pre => State (G) = Valid;

   function State (G : Read_Grant) return Result_Kind;
   function Slice (G : Read_Grant) return Slice_Rec
     with Pre => State (G) = Valid;

   function Write_Grant_In_Progress (This : Framed_Buffer) return Boolean
     with Ghost;
   function Read_Grant_In_Progress (This : Framed_Buffer) return Boolean
     with Ghost;

private

   subtype Header_Count is Framed_Count range 0 .. Max_Frame_Header_Size;

   type Framed_Buffer (Size : Buffer_Size) is limited record
      Buffer    : BBqueue.Buffers.Buffer (Size);

      Current_Read_Size : Framed_Count := 0;
      --  This stores the size of the current read frame, between Read and
      --  Release. It is used to prove that the release size (Header_Size +
      --  Current_Read_Size) doesn't overflow.
   end record;

   function Empty_Slicerec return Slice_Rec
   is (0, System.Null_Address);

   type Write_Grant is limited record
      Grant : BBqueue.Buffers.Write_Grant;
      Header_Size : Header_Count := 0;
   end record;

   type Read_Grant is limited record
      Grant : BBqueue.Buffers.Read_Grant;
      Header_Size : Header_Count := 0;
   end record;

   function State (G : Write_Grant) return Result_Kind
   is (BBqueue.Buffers.State (G.Grant));
   function Empty return Write_Grant
   is (Grant => BBqueue.Buffers.Empty, others => <>);
   function Slice (G : Write_Grant) return Slice_Rec
   is (BBqueue.Buffers.Slice (G.Grant));

   function State (G : Read_Grant) return Result_Kind
   is (BBqueue.Buffers.State (G.Grant));
   function Empty return Read_Grant
   is (Grant => BBqueue.Buffers.Empty, others => <>);
   function Slice (G : Read_Grant) return Slice_Rec
   is (BBqueue.Buffers.Slice (G.Grant));

   -----------------------------
   -- Write_Grant_In_Progress --
   -----------------------------

   function Write_Grant_In_Progress (This : Framed_Buffer) return Boolean
   is (Write_Grant_In_Progress (This.Buffer));

   ----------------------------
   -- Read_Grant_In_Progress --
   ----------------------------

   function Read_Grant_In_Progress (This : Framed_Buffer) return Boolean
   is (Read_Grant_In_Progress (This.Buffer));

end BBqueue.Buffers.framed;
