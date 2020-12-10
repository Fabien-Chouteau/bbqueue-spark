package body Array_Slices
with SPARK_Mode
is

   ------------
   -- Create --
   ------------

   function Create (Addr : System.Address; Size : Count_Type) return Slice is
   begin
      if Size = 0 then
         return Empty_Slice;
      end if;

      return Ret : Slice do
         Ret.F := Index_Type'First;
         Ret.L := Index_Type'First + Index_Type (Size) - 1;
         Ret.Addr := Addr;
      end return;
   end Create;

   -----------
   -- Empty --
   -----------

   function Empty (This : Slice) return Boolean
   is (This.L < This.F);

   -----------
   -- First --
   -----------

   function First (This : Slice) return Index_Type
   is (This.F);

   ----------
   -- Last --
   ----------

   function Last (This : Slice) return Index_Type
   is (This.L);

   ------------
   -- Length --
   ------------

   function Length (This : Slice) return Count_Type
   is (if Empty (This)
       then 0
       else Count_Type (This.L - This.F + 1));

   --------------
   -- In_Range --
   --------------

   function In_Range (This : Slice; Index : Index_Type) return Boolean
   is (Index in This.F .. This.L);

   ---------
   -- Set --
   ---------

   procedure Set (This : in out Slice; Index : Index_Type; Data : Element_Type) is
      pragma SPARK_Mode (Off);
      Arr : Virt_Arr with Address => This.Addr;
   begin
      Arr (Index) := Data;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (This : in out Slice; From : Index_Type; Data : Array_Type) is
      pragma SPARK_Mode (Off);
      Arr : Virt_Arr with Address => This.Addr;
   begin
      Arr (From .. From + Data'Length - 1) := Data;
   end Set;

   ----------
   -- Read --
   ----------

   procedure Read (This : Slice; From : Index_Type; Data : out Array_Type) is
      pragma SPARK_Mode (Off);
      Arr : Virt_Arr with Address => This.Addr;
   begin
      Data := Arr (From .. From + Data'Length - 1);
   end Read;

   -------------
   -- Element --
   -------------

   function Element (This : Slice; Index : Index_Type) return Element_Type is
      pragma SPARK_Mode (Off);
      Arr : Virt_Arr with Address => This.Addr;
   begin
      return Arr (Index);
   end Element;

   ----------
   -- Addr --
   ----------

   function Addr (This : Slice; Index : Index_Type) return System.Address is
      pragma SPARK_Mode (Off);
      Arr : Virt_Arr with Address => This.Addr;
   begin
      return Arr (Index)'Address;
   end Addr;

   ----------
   -- Addr --
   ----------

   function Addr (This : Slice) return System.Address
   is (Addr (This, This.F));

   package body Callbacks is
      pragma SPARK_Mode (Off);

      ----------
      -- Read --
      ----------

      procedure Read
        (This : Slice;
         Callback : not null access procedure (Data : Array_Type))
      is
         pragma SPARK_Mode (Off);
         Arr : Virt_Arr with Address => This.Addr;
      begin
         Callback (Arr (This.F .. This.L));
      end Read;

      -----------
      -- Write --
      -----------

      procedure Write
        (This : in out Slice;
         Callback : not null access procedure (Data : out Array_Type))
      is
         pragma SPARK_Mode (Off);
         Arr : Virt_Arr with Address => This.Addr;
      begin
         Callback (Arr (This.F .. This.L));
      end Write;

      ----------------
      -- Read_Write --
      ----------------

      procedure Read_Write
        (This : in out Slice;
         Callback : not null access procedure (Data : in out Array_Type))
      is
         pragma SPARK_Mode (Off);
         Arr : Virt_Arr with Address => This.Addr;
      begin
         Callback (Arr (This.F .. This.L));
      end Read_Write;
   end Callbacks;

end Array_Slices;
