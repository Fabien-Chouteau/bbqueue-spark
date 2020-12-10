with System; use System;

generic
   type Count_Type is mod <>;
   type Index_Type is mod <>;
   type Element_Type is private;
   type Array_Type is array (Index_Type range <>) of Element_Type;
package Array_Slices
with Preelaborate,
     SPARK_Mode
is

   type Slice is private;

   function Empty_Slice return Slice;

   function Create (Addr : System.Address;
                    Size : Count_Type)
                    return Slice
     with Pre => Size < Count_Type (Index_Type'Last)
                 and then Addr /= System.Null_Address,
         Post => (if Size = 0
                  then Empty (Create'Result))
                  and then
                  (if not Empty (Create'Result)
                   then First (Create'Result) = Index_Type'First
                   and then Last (Create'Result) = Index_Type'First + Index_Type (Size) - 1
                   and then Length (Create'Result) = Size);

   function Empty (This : Slice) return Boolean
     with Post => Empty'Result = (Last (This) < First (This));

   function First (This : Slice) return Index_Type;

   function Last (This : Slice) return Index_Type;

   function Length (This : Slice) return Count_Type
     with Post => (if Empty (This) then Length'Result = 0);

   function In_Range (This : Slice; Index : Index_Type) return Boolean
     with Post => (if Empty (This)
                   then In_Range'Result = False
                   else In_Range'Result = (Index in First (This) .. Last (This)));

   procedure Set (This : in out Slice; Index : Index_Type; Data : Element_Type)
     with Pre => In_Range (This, Index);

   function Element (This : Slice; Index : Index_Type) return Element_Type
     with Pre => In_Range (This, Index);

   procedure Set (This : in out Slice; From : Index_Type; Data : Array_Type)
     with Pre => Data'Length /= 0
     and then Data'Last < Index_Type'Last
     and then Index_Type'Last - From > Data'Length
     and then Data'Length < Length (This)
     and then In_Range (This, From)
     and then In_Range (This, From + Data'Length - 1);

   procedure Read (This : Slice; From : Index_Type; Data : out Array_Type)
     with Pre =>  Data'Length /= 0
     and then Data'Last < Index_Type'Last
     and then Index_Type'Last - From > Data'Length
     and then Data'Length < Length (This)
     and then In_Range (This, From)
     and then In_Range (This, From + Data'Length - 1);

   function Addr (This : Slice; Index : Index_Type) return System.Address
     with Pre => In_Range (This, Index);

   function Addr (This : Slice) return System.Address
     with Pre => not Empty (This),
         Post => Addr'Result = Addr (This, First (This));

   --  Callbacks --

   package Callbacks is
      pragma SPARK_Mode (Off);
      procedure Read (This : Slice;
                      Callback : not null access procedure (Data : Array_Type));

      procedure Write (This : in out Slice;
                       Callback : not null access procedure (Data : out Array_Type));

      procedure Read_Write (This : in out Slice;
                            Callback : not null access procedure (Data : in out Array_Type));
   end Callbacks;

private

   subtype Virt_Arr is Array_Type (Index_Type);

   type Slice is record
      Addr : System.Address;
      F, L : Index_Type;
   end record;

   -----------------
   -- Empty_Slice --
   -----------------

   function Empty_Slice return Slice
   is (System.Null_Address, Index_Type'Last, Index_Type'First);

   pragma Inline (Empty);
   pragma Inline (First);
   pragma Inline (Last);
   pragma Inline (Length);
   pragma Inline (In_Range);
   pragma Inline (Set);
   pragma Inline (Element);
   pragma Inline (Read);
   pragma Inline (Addr);
end Array_Slices;
