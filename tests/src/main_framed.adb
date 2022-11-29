with Ada.Text_IO; use Ada.Text_IO;

with System.Storage_Elements; use System.Storage_Elements;
with BBqueue;
with BBqueue.Buffers.framed;
with System; use System;

procedure Main_Framed
with SPARK_Mode
is
   use type BBqueue.Result_Kind;

   Q : aliased BBqueue.Buffers.framed.Framed_Buffer (60);

   procedure Fill_With_CB (Request, Actual : BBqueue.Count;
                           Val : Storage_Element);
   procedure Print_Content_With_CB;

   ------------------
   -- Fill_With_CB --
   ------------------

   procedure Fill_With_CB (Request, Actual : BBqueue.Count;
                           Val             : Storage_Element)
   is
      pragma SPARK_Mode (Off);

      procedure Process_Write (Data      : out Storage_Array;
                               To_Commit : out BBqueue.Count);

      procedure Process_Write (Data      : out Storage_Array;
                               To_Commit : out BBqueue.Count)
      is
      begin
         Put_Line ("Fill" & Actual'Img & " bytes.");
         Data (Data'First .. Data'First + Actual - 1) := (others => Val);
         To_Commit := Actual;
      end Process_Write;

      procedure Write is new BBqueue.Buffers.framed.Write_CB (Process_Write);
      Result : BBqueue.Result_Kind;
   begin
      Write (Q, Request, Result);
      if Result /= BBqueue.Valid then
         Put_Line ("Write failed: " & Result'Img);
      end if;
   end Fill_With_CB;

   ---------------------------
   -- Print_Content_With_CB --
   ---------------------------

   procedure Print_Content_With_CB is
      procedure Process_Read (Data : Storage_Array);

      procedure Process_Read (Data : Storage_Array) is
      begin
         Put ("Print" & Data'Length'Img & " bytes -> ");
         for Elt of Data loop
            Put (Elt'Img);
         end loop;
         New_Line;
      end Process_Read;

      procedure Read is new BBqueue.Buffers.framed.Read_CB (Process_Read);
      Result : BBqueue.Result_Kind;
   begin
      Read (Q, Result);
      if Result /= BBqueue.Valid then
         Put_Line ("Read failed: " & Result'Img);
      end if;
   end Print_Content_With_CB;

   V : Storage_Element := 1;
begin

   Put_Line ("Count'Object_Size:" & BBqueue.Count'Object_Size'Img);

   for X in BBqueue.Count range 1 .. 4 loop
      Put_Line ("-- Loop" & X'Img & " --");
      Fill_With_CB (10, X, V);
      Fill_With_CB (20, X * 2, V * 2);
      V := V + 1;
      Print_Content_With_CB;
      Print_Content_With_CB;
   end loop;
end Main_Framed;
