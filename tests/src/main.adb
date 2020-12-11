with Ada.Text_IO; use Ada.Text_IO;

with System.Storage_Elements; use System.Storage_Elements;
with BBqueue;
with System; use System;

procedure Main
with SPARK_Mode
is
   use type BBqueue.Result_Kind;

   Buffer : Storage_Array (0 .. 34) := (others => 0);
   Q : aliased BBqueue.Offsets_Only (Buffer'Length);

   procedure Fill (WG : BBqueue.Write_Grant;
                   Val : Storage_Element)
     with Pre => BBqueue.State (WG) = BBqueue.Valid
     and then BBqueue.Valid_Slice (Q, BBqueue.Slice (WG));

   procedure Print (G : BBqueue.Write_Grant);
   procedure Print (G : BBqueue.Read_Grant);
   procedure Print_Content (RG : BBqueue.Read_Grant)
     with Pre => BBqueue.State (RG) = BBqueue.Valid
     and then BBqueue.Valid_Slice (Q, BBqueue.Slice (RG));
   procedure Print_Buffer;

   ----------
   -- Fill --
   ----------

   procedure Fill (WG : BBqueue.Write_Grant;
                   Val : Storage_Element)
   is
      S : constant BBqueue.Slice_Rec := BBqueue.Slice (WG);
   begin
      Buffer (Buffer'First + S.Start_Offset .. Buffer'First + S.End_Offset)
                := (others => Val);
   end Fill;

   -----------
   -- Print --
   -----------

   procedure Print (G : BBqueue.Write_Grant) is
   begin
      Put ("Write Grant - " & BBqueue.State (G)'Img);
      if BBqueue.State (G) = BBqueue.Valid then
         Put_Line ("  Size:" & BBqueue.Slice (G).Length'Img);
      else
         New_Line;
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (G : BBqueue.Read_Grant) is
   begin
      Put ("Read Grant - " & BBqueue.State (G)'Img);
      if BBqueue.State (G) = BBqueue.Valid then
         Put_Line ("  Size:" & BBqueue.Slice (G).Length'Img);
      else
         New_Line;
      end if;
   end Print;

   -------------------
   -- Print_Content --
   -------------------

   procedure Print_Content (RG : BBqueue.Read_Grant) is
      S : constant BBqueue.Slice_Rec := BBqueue.Slice (RG);
   begin
      Put ("Print " & S.Length'Img & " bytes -> ");
      for Elt of Buffer (Buffer'First + S.Start_Offset .. Buffer'First + S.End_Offset) loop
         Put (Elt'Img);
      end loop;
      New_Line;
   end Print_Content;

   ------------------
   -- Print_Buffer --
   ------------------

   procedure Print_Buffer is
   begin
      Put ("Buffer => ");
      for Elt of Buffer loop
         Put (Elt'Img);
      end loop;
      New_Line;
   end Print_Buffer;

   WG : BBqueue.Write_Grant := BBqueue.Empty;
   RG : BBqueue.Read_Grant := BBqueue.Empty;

   V : Storage_Element := 1;
begin

   for X in 1 .. 7 loop
      Put_Line ("-- Loop" & X'Img & " --");

      Print_Buffer;

      BBqueue.Grant (Q, WG, 10);
      if BBqueue.State (WG) /= BBqueue.Valid then
         exit;
      end if;

      Put ("BBqueue.Grant (Q, 10) -> ");
      Print (WG);
      Print_Buffer;
      Put_Line ("Fill (WG, " & V'Img & ")");
      Fill (WG, V);
      V := V + 1;
      Print_Buffer;

      BBqueue.Commit (Q, WG, 10);
      Put ("BBqueue.Commit (WG, 10); ->");
      Print (WG);
      Print_Buffer;

      BBqueue.Read (Q, RG);
      if BBqueue.State (RG) /= BBqueue.Valid then
         exit;
      end if;

      Put ("BBqueue.Read (Q); -> ");
      Print (RG);
      Print_Content (RG);
      Print_Buffer;

      BBqueue.Release (Q, RG);
      Put ("BBqueue.Release (RG); -> ");
      Print (RG);
      Print_Buffer;

      pragma Assert (BBqueue.State (WG) = BBqueue.Empty);
      pragma Assert (BBqueue.State (RG) = BBqueue.Empty);
   end loop;
end Main;
