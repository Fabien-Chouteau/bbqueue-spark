with System;

package body Atomic is

   ----------
   -- Init --
   ----------

   function Init (Val : Boolean) return Flag is
      Ret : aliased Flag := 0;
      Unused : Boolean;
   begin
      if Val then
         Test_And_Set (Ret, Unused);
      else
         Clear (Ret);
      end if;
      return Ret;
   end Init;

   ---------
   -- Set --
   ---------

   function Set (This  : aliased Flag;
                 Order : Mem_Order := Seq_Cst)
                 return Boolean
   is
      function Intrinsic
        (Ptr   : System.Address;
         Model : Integer)
         return Flag;
      pragma Import (Intrinsic, Intrinsic, "__atomic_load_1");
   begin
      return Intrinsic (This'Address, Order'Enum_Rep) /= 0;
   end Set;

   ------------------
   -- Test_And_Set --
   ------------------

   procedure Test_And_Set (This   : aliased in out Flag;
                           Result : out Boolean;
                           Order  : Mem_Order := Seq_Cst)
   is
      function Intrinsic
        (Ptr   : System.Address;
         Model : Integer)
         return Boolean;
      pragma Import (Intrinsic, Intrinsic, "__atomic_test_and_set");
   begin
      Result := Intrinsic (This'Address, Order'Enum_Rep);
   end Test_And_Set;

   --  ------------------
   --  -- Test_And_Set --
   --  ------------------
   --
   --  function Test_And_Set (This  : aliased in out Flag;
   --                         Order : Mem_Order := Seq_Cst)
   --                         return Boolean
   --  is
   --     function Intrinsic
   --       (Ptr   : System.Address;
   --        Model : Integer)
   --        return Boolean;
   --     pragma Import (Intrinsic, Intrinsic, "__atomic_test_and_set");
   --  begin
   --     return Intrinsic (This'Address, Order'Enum_Rep);
   --  end Test_And_Set;
   --
   -----------
   -- Clear --
   -----------

   procedure Clear (This : aliased in out Flag;
                    Order : Mem_Order := Seq_Cst)
   is
      procedure Intrinsic
        (Ptr   : System.Address;
         Model : Integer);
      pragma Import (Intrinsic, Intrinsic, "__atomic_clear");
   begin
      Intrinsic (This'Address, Order'Enum_Rep);
   end Clear;

end Atomic;
