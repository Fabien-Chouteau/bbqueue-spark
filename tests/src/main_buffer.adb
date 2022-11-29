with Ada.Text_IO; use Ada.Text_IO;

with System.Storage_Elements; use System.Storage_Elements;
with BBqueue;
with BBqueue.Buffers;
with System; use System;

procedure Main_Buffer
with SPARK_Mode
is
   use type BBqueue.Result_Kind;

   Q : aliased BBqueue.Buffers.Buffer (35);

   procedure Fill (WG : BBqueue.Buffers.Write_Grant;
                   Val : Storage_Element)
     with Pre => BBqueue.Buffers.State (WG) = BBqueue.Valid;

   procedure Fill_With_CB (Size : BBqueue.Count;
                           Val : Storage_Element);

   procedure Print_Content (RG : BBqueue.Buffers.Read_Grant)
     with Pre => BBqueue.Buffers.State (RG) = BBqueue.Valid;

   procedure Print_Content_With_CB;

   ----------
   -- Fill --
   ----------

   procedure Fill (WG : BBqueue.Buffers.Write_Grant;
                   Val : Storage_Element)
   is
      pragma SPARK_Mode (Off);

      S : constant BBqueue.Buffers.Slice_Rec := BBqueue.Buffers.Slice (WG);
      Arr : Storage_Array (1 .. S.Length) with Address => S.Addr;
   begin
      Put_Line ("Fill" & S.Length'Img & " bytes.");
      Arr := (others => Val);
   end Fill;

   ------------------
   -- Fill_With_CB --
   ------------------

   procedure Fill_With_CB (Size : BBqueue.Count; Val : Storage_Element) is

      pragma SPARK_Mode (Off);

      procedure Process_Write (Data      : out Storage_Array;
                               To_Commit : out BBqueue.Count);

      procedure Process_Write (Data      : out Storage_Array;
                               To_Commit : out BBqueue.Count)
      is
      begin
         Put_Line ("Fill" & Data'Length'Img & " bytes.");
         Data := (others => Val);
         To_Commit := Data'Length;
      end Process_Write;

      procedure Write is new BBqueue.Buffers.Write_CB (Process_Write);
      Result : BBqueue.Result_Kind;
   begin
      Write (Q, Size, Result);
      if Result /= BBqueue.Valid then
         Put_Line ("Write failed: " & Result'Img);
      end if;
   end Fill_With_CB;

   -------------------
   -- Print_Content --
   -------------------

   procedure Print_Content (RG : BBqueue.Buffers.Read_Grant) is
      pragma SPARK_Mode (Off);
      S : constant BBqueue.Buffers.Slice_Rec := BBqueue.Buffers.Slice (RG);
      Arr : Storage_Array (1 .. S.Length) with Address => S.Addr;
   begin
      Put ("Print" & S.Length'Img & " bytes -> ");
      for Elt of Arr loop
         Put (Elt'Img);
      end loop;
      New_Line;
   end Print_Content;

   ---------------------------
   -- Print_Content_With_CB --
   ---------------------------

   procedure Print_Content_With_CB is
      procedure Process_Read (Data       :     Storage_Array;
                              To_Release : out BBqueue.Count);

      procedure Process_Read (Data       :     Storage_Array;
                              To_Release : out BBqueue.Count)
      is
      begin
         Put ("Print" & Data'Length'Img & " bytes -> ");
         for Elt of Data loop
            Put (Elt'Img);
         end loop;
         New_Line;
         To_Release := Data'Length;
      end Process_Read;

      procedure Read is new BBqueue.Buffers.Read_CB (Process_Read);
      Result : BBqueue.Result_Kind;
   begin
      Read (Q, Result);
      if Result /= BBqueue.Valid then
         Put_Line ("Read failed: " & Result'Img);
      end if;
   end Print_Content_With_CB;

   WG : BBqueue.Buffers.Write_Grant := BBqueue.Buffers.Empty;
   RG : BBqueue.Buffers.Read_Grant := BBqueue.Buffers.Empty;

   V : Storage_Element := 1;
begin

   for X in 1 .. 4 loop
      Put_Line ("-- Loop" & X'Img & " --");

      BBqueue.Buffers.Grant (Q, WG, 10);
      if BBqueue.Buffers.State (WG) /= BBqueue.Valid then
         exit;
      end if;
      Put_Line ("BBqueue.Buffers.Grant (Q, 10) -> ");
      Put_Line ("Fill (WG, " & V'Img & ")");
      Fill (WG, V);
      V := V + 1;

      BBqueue.Buffers.Commit (Q, WG, 10);
      Put_Line ("BBqueue.Buffers.Commit (WG, 10); ->");

      BBqueue.Buffers.Read (Q, RG);
      if BBqueue.Buffers.State (RG) /= BBqueue.Valid then
         exit;
      end if;
      Put ("BBqueue.Buffers.Read (Q, RG); -> ");
      Print_Content (RG);

      BBqueue.Buffers.Release (Q, RG);
      Put_Line ("BBqueue.Buffers.Release (Q, RG); -> ");

      pragma Assert (BBqueue.Buffers.State (WG) = BBqueue.Empty);
      pragma Assert (BBqueue.Buffers.State (RG) = BBqueue.Empty);
   end loop;

   for X in 5 .. 7 loop
      Put_Line ("-- Loop" & X'Img & " with callbacks --");
      Fill_With_CB (5, V);
      Fill_With_CB (5, V + 1);
      V := V + 1;
      Print_Content_With_CB;
      Print_Content_With_CB;
   end loop;
end Main_Buffer;
