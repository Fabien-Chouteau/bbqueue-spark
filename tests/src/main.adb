with Ada.Text_IO; use Ada.Text_IO;

with System.Storage_Elements; use System.Storage_Elements;
with BBqueue;
with System; use System;

with Interfaces;

procedure Main
with SPARK_Mode
is
   use type BBqueue.Result_Kind;

   procedure Fill (WG : BBqueue.Write_Grant;
                   Val : Storage_Element)
     with Pre => BBqueue.State (WG) = BBqueue.Valid;
   procedure Print (G : BBqueue.Write_Grant);
   procedure Print (G : BBqueue.Read_Grant);
   procedure Print_Content (RG : BBqueue.Read_Grant)
     with Pre => BBqueue.State (RG) = BBqueue.Valid;

   ----------
   -- Fill --
   ----------

   procedure Fill (WG : BBqueue.Write_Grant;
                   Val : Storage_Element)
   is
      pragma SPARK_Mode (Off);

      procedure Sub_Fill (Data : out BBqueue.Storage_Array);

      procedure Sub_Fill (Data : out BBqueue.Storage_Array) is
      begin
         Data := (others => Interfaces.Unsigned_8 (Val));
      end Sub_Fill;

      S : BBqueue.Slices.Slice := BBqueue.Slice (WG);
   begin
      BBqueue.Slices.Callbacks.Write (S, Sub_Fill'Access);
   end Fill;

   -----------
   -- Print --
   -----------

   procedure Print (G : BBqueue.Write_Grant) is
   begin
      Put ("Write Grant - " & BBqueue.State (G)'Img);
      if BBqueue.State (G) = BBqueue.Valid then
         Put_Line ("  Size:" & BBqueue.Slices.Length (BBqueue.Slice (G))'Img);
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
         Put_Line ("  Size:" & BBqueue.Slices.Length (BBqueue.Slice (G))'Img);
      else
         New_Line;
      end if;
   end Print;

   -------------------
   -- Print_Content --
   -------------------

   procedure Print_Content (RG : BBqueue.Read_Grant) is
      pragma SPARK_Mode (Off);

      procedure Sub_Print (Data : BBqueue.Storage_Array);

      procedure Sub_Print (Data : BBqueue.Storage_Array) is
      begin
         for Elt of Data loop
            Put (Elt'Img);
         end loop;
         New_Line;
      end Sub_Print;

      S : constant BBqueue.Slices.Slice := BBqueue.Slice (RG);
   begin
      Put ("Print " & BBqueue.Slices.Length (S)'Img & " bytes -> ");
      BBqueue.Slices.Callbacks.Read (S, Sub_Print'Access);
   end Print_Content;

   Q : aliased BBqueue.Buffer (35);

   WG : BBqueue.Write_Grant := BBqueue.Empty;
   RG : BBqueue.Read_Grant := BBqueue.Empty;

   V : Storage_Element := 1;
begin

   for X in 1 .. 7 loop
      Put_Line ("-- Loop" & X'Img & " --");

      --  BBqueue.Print (Q);

      BBqueue.Grant (Q, WG, 10);
      if BBqueue.State (WG) /= BBqueue.Valid then
         exit;
      end if;

      Put ("BBqueue.Grant (Q, 10) -> ");
      Print (WG);
      --  BBqueue.Print (Q);
      Put_Line ("Fill (WG, " & V'Img & ")");
      Fill (WG, V);
      V := V + 1;
      --  BBqueue.Print (Q);

      BBqueue.Commit (Q, WG, 10);
      Put ("BBqueue.Commit (WG, 10); ->");
      Print (WG);
      --  BBqueue.Print (Q);

      BBqueue.Read (Q, RG);
      if BBqueue.State (RG) /= BBqueue.Valid then
         exit;
      end if;

      Put ("BBqueue.Read (Q); -> ");
      Print (RG);
      Print_Content (RG);
      --  BBqueue.Print (Q);

      BBqueue.Release (Q, RG);
      Put ("BBqueue.Release (RG); -> ");
      Print (RG);
      --  BBqueue.Print (Q);

      pragma Assert (BBqueue.State (WG) = BBqueue.Empty);
      pragma Assert (BBqueue.State (RG) = BBqueue.Empty);
   end loop;
end Main;
