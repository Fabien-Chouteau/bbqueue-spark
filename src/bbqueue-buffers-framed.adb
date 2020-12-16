package body BBqueue.Buffers.framed
with SPARK_Mode
is

   pragma Compile_Time_Error ((Count'Object_Size mod System.Storage_Unit) /= 0,
                             "Invalid Object_Size for Count");

   procedure Write_Header (After_Hdr_Addr : System.Address;
                           Hdr_Size       : Header_Count;
                           Value          : Count);

   procedure Read_Header (Before_Hdr_Addr : System.Address;
                          Hdr_Size        : out Header_Count;
                          Frame_Size      : out Framed_Count)
     with Post => Hdr_Size = (Count'Object_Size / System.Storage_Unit);

   function Header_Size (Unused : Count) return Header_Count
   is (Count'Object_Size / System.Storage_Unit)

     with Post => Header_Size'Result in 1 .. 9;
   --  TODO: The size of the header can be optimized using variable-length
   --  encoding.

   ------------------
   -- Write_Header --
   ------------------

   procedure Write_Header (After_Hdr_Addr : System.Address;
                           Hdr_Size       : Header_Count;
                           Value          : Count)
   is
      pragma SPARK_Mode (Off);
      Header : Count
        with Address =>
          To_Address (To_Integer (After_Hdr_Addr) - Integer_Address (Hdr_Size));
   begin
      Header := Value;
   end Write_Header;

   -----------------
   -- Read_Header --
   -----------------

   procedure Read_Header (Before_Hdr_Addr : System.Address;
                          Hdr_Size        : out Header_Count;
                          Frame_Size      : out Framed_Count)
   is
      pragma SPARK_Mode (Off);
      Header : Count
        with Address => Before_Hdr_Addr;
   begin
      Frame_Size := Header;
      Hdr_Size := Header_Size (Frame_Size);
   end Read_Header;

   -----------
   -- Grant --
   -----------

   procedure Grant (This : in out Framed_Buffer;
                    G    : in out Write_Grant;
                    Size :        Framed_Count)
   is
      Hdr_Size : constant Count := Header_Size (Size);
   begin
      if Size = 0 then
         BBqueue.Buffers.Grant (This.Buffer, G.Grant, Size);
         G.Header_Size := 0;
         return;
      end if;

      --  Save the worst case header size
      G.Header_Size := Hdr_Size;

      --  Request Size + worst case header size
      BBqueue.Buffers.Grant (This.Buffer, G.Grant, Size + Hdr_Size);

      if State (G) = Valid then

         pragma Assert (G.Grant.Slice.Length = Size + Hdr_Size);

         --  Change the slice to skip the header
         G.Grant.Slice.Length := G.Grant.Slice.Length - Hdr_Size;
         G.Grant.Slice.Addr :=
           To_Address (To_Integer (G.Grant.Slice.Addr) + Integer_Address (Hdr_Size));
      else
         --  Grant failed, no header
         G.Header_Size := 0;
      end if;
   end Grant;

   ------------
   -- Commit --
   ------------

   procedure Commit (This : in out Framed_Buffer;
                     G    : in out Write_Grant;
                     Size :        Framed_Count := Framed_Count'Last)
   is
   begin
      if Size = 0 then
         --  Nothing to commit
         BBqueue.Buffers.Commit (This.Buffer, G.Grant, 0);
      else

         --  Write the header in the buffer
         Write_Header (Slice (G.Grant).Addr, G.Header_Size, Size);

         --  Commit header + data
         BBqueue.Buffers.Commit (This.Buffer, G.Grant, Size + G.Header_Size);
      end if;

      if State (G) = Empty then
         G.Header_Size := 0;
      end if;
   end Commit;

   --------------
   -- Write_CB --
   --------------

   procedure Write_CB
     (This   : in out Framed_Buffer;
      Size   :        Framed_Count;
      Result :    out Result_Kind)
   is
      G : Write_Grant := Empty;
   begin
      Grant (This, G, Size);
      Result := State (G);

      if Result = Valid then
         declare
            S : constant Slice_Rec := Slice (G);
            B : Storage_Array (1 .. S.Length)
              with Address => S.Addr;
            To_Commit : Count;
         begin
            Process_Write (B, To_Commit);

            Commit (This, G, To_Commit);

            pragma Assert (State (G) = Empty);
         end;
      end if;
   end Write_CB;

   ----------
   -- Read --
   ----------

   procedure Read (This : in out Framed_Buffer; G : in out Read_Grant) is
      Frame_Size : Framed_Count;
      Hdr_Size : Header_Count;
   begin
      BBqueue.Buffers.Read (This.Buffer, G.Grant);

      if State (G) = Valid then

         --  Get header size and value from the buffer
         Read_Header (Slice (G.Grant).Addr, Hdr_Size, Frame_Size);
         G.Header_Size := Hdr_Size;

         --  Change the slice to skip the header and set the actuall value of
         --  the frame.
         G.Grant.Slice.Length := Frame_Size;
         G.Grant.Slice.Addr :=
           To_Address (To_Integer (G.Grant.Slice.Addr) + Integer_Address (Hdr_Size));

         This.Current_Read_Size := Frame_Size;
      end if;
   end Read;

   -------------
   -- Release --
   -------------

   procedure Release (This : in out Framed_Buffer; G : in out Read_Grant) is
   begin
      BBqueue.Buffers.Release (This.Buffer,
                               G.Grant,
                               G.Header_Size + This.Current_Read_Size);
      G.Header_Size := 0;
   end Release;

   -------------
   -- Read_CB --
   -------------

   procedure Read_CB (This : in out Framed_Buffer; Result : out Result_Kind) is
      G : Read_Grant := Empty;

      procedure Call_CB (Addr   : System.Address;
                         Length : Framed_Count);

      procedure Call_CB (Addr   : System.Address;
                         Length : Framed_Count)
      is
         pragma SPARK_Mode (Off);
         B : Storage_Array (1 .. Length)
           with Address => Addr;
      begin
         Process_Read (B);
      end Call_CB;

   begin
      Read (This, G);
      Result := State (G);

      if Result = Valid then
         Call_CB (Slice (G).Addr, This.Current_Read_Size);

         Release (This, G);

         pragma Assert (State (G) = Empty);
      end if;
   end Read_CB;

end BBqueue.Buffers.framed;
