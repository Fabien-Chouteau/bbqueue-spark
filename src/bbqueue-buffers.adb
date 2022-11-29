package body BBqueue.Buffers
with SPARK_Mode
is

   pragma Warnings (Off, "lower bound test optimized away");

   function Get_Addr (This   : Buffer;
                      Offset : Buffer_Offset)
                      return System.Address
     with Pre => Offset in 0 .. This.Buf'Last - 1;

   --------------
   -- Get_Addr --
   --------------

   function Get_Addr (This   : Buffer;
                      Offset : Buffer_Offset)
                      return System.Address
   is
      pragma SPARK_Mode (Off);
   begin
      return This.Buf (This.Buf'First + Offset)'Address;
   end Get_Addr;

   -----------
   -- Grant --
   -----------

   procedure Grant (This : in out Buffer;
                    G    : in out Write_Grant;
                    Size : Count)
   is
   begin
      BBqueue.Grant (This.Offsets, G.Offsets_Grant, Size);

      if G.Offsets_Grant.Result = Valid then
         G.Slice.Length := G.Offsets_Grant.Slice.Length;
         G.Slice.Addr := Get_Addr (This, G.Offsets_Grant.Slice.From);
      end if;
   end Grant;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (This : in out Buffer; G : in out Write_Grant; Size : Count := Count'Last)
   is
   begin
      BBqueue.Commit (This.Offsets, G.Offsets_Grant, Size);
   end Commit;

   --------------
   -- Write_CB --
   --------------

   procedure Write_CB (This   : in out Buffer;
                       Size   :        Count;
                       Result :    out Result_Kind)
   is
      G : Write_Grant := Empty;
   begin
      Grant (This, G, Size);
      Result := State (G);

      if Result = Valid then
         declare
            S : constant BBqueue.Slice_Rec := BBqueue.Slice (G.Offsets_Grant);
            B : Storage_Array renames This.Buf;
            To_Commit : Count;
         begin
            Process_Write (B (B'First + S.From .. B'First + S.To),
                           To_Commit);

            Commit (This, G, To_Commit);

            pragma Assert (State (G) = Empty);
         end;
      end if;
   end Write_CB;

   ----------
   -- Read --
   ----------

   procedure Read (This : in out Buffer;
                   G    : in out Read_Grant;
                   Max  :        Count := Count'Last)
   is
   begin
      BBqueue.Read (This.Offsets, G.Offsets_Grant, Max);

      if G.Offsets_Grant.Result = Valid then
         G.Slice.Length := G.Offsets_Grant.Slice.Length;
         G.Slice.Addr := Get_Addr (This, G.Offsets_Grant.Slice.From);
      end if;
   end Read;

   -------------
   -- Release --
   -------------

   procedure Release
     (This : in out Buffer; G : in out Read_Grant; Size : Count := Count'Last)
   is
   begin
      BBqueue.Release (This.Offsets, G.Offsets_Grant, Size);
   end Release;

   -------------
   -- Read_CB --
   -------------

   procedure Read_CB (This   : in out Buffer;
                      Result :    out Result_Kind)
   is
      pragma SPARK_Mode (Off);

      G : Read_Grant := Empty;
   begin
      Read (This, G);
      Result := State (G);

      if Result = Valid then
         declare
            S : constant BBqueue.Slice_Rec := BBqueue.Slice (G.Offsets_Grant);
            B : Storage_Array renames This.Buf;
            To_Release : Count;
         begin
            Process_Read (B (B'First + S.From .. B'First + S.To),
                          To_Release);

            Release (This, G, To_Release);

            pragma Assert (State (G) = Empty);
         end;
      end if;

   end Read_CB;

end BBqueue.Buffers;
