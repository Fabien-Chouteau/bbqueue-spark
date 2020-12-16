generic
   type T is range <>;
package Atomic.Generic_Signed32
with Preelaborate, Spark_Mode => On
is
   --  Based on GCC atomic built-ins. See:
   --  https://gcc.gnu.org/onlinedocs/gcc/_005f_005fatomic-Builtins.html
   --
   --  The specification is exactly the same for all sizes of data (8, 16, 32,
   --  64).

   type Instance is limited private;
   --  This type is limited and private, it can only be manipulated using the
   --  primitives below.

   function Init (Val : T) return Instance
     with Post => Value (Init'Result) = Val;
   --  Can be used to initialize an atomic instance:
   --
   --  A : Atomic.Unsigned_8.Instance := Atomic.Unsigned_8.Init (0);

   function Value (This : Instance) return T
     with Ghost;
   --  Ghost function to get the value of an instance without needing it
   --  aliased. This function can be used in contracts for instance.
   --  This doesn't use the atomic built-ins.

   function Load (This  : aliased Instance;
                  Order : Mem_Order := Seq_Cst)
                  return T
     with Pre  => Order in Relaxed | Consume | Acquire | Seq_Cst,
          Post => Load'Result = Value (This);

   procedure Store (This  : aliased in out Instance;
                    Val   : T;
                    Order : Mem_Order := Seq_Cst)
     with Pre  => Order in Relaxed | Release | Seq_Cst,
          Post => Value (This) = Val;

   procedure Exchange (This  : aliased in out Instance;
                       Val   : T;
                       Old   : out T;
                       Order : Mem_Order := Seq_Cst)
     with Pre  => Order in Relaxed | Acquire | Release | Acq_Rel | Seq_Cst,
          Post => Old = Value (This)'Old and then Value (This) = Val;

   procedure Compare_Exchange (This          : aliased in out Instance;
                               Expected      : T;
                               Desired       : T;
                               Weak          : Boolean;
                               Success       : out Boolean;
                               Success_Order : Mem_Order := Seq_Cst;
                               Failure_Order : Mem_Order := Seq_Cst)
     with Pre  => Failure_Order in Relaxed | Consume | Acquire | Seq_Cst
                  and then
                  not Stronger (Failure_Order, Success_Order),
          Post => Success = (Value (This)'Old = Expected)
                  and then
                  (if Success then Value (This) = Desired);

   procedure Add (This  : aliased in out Instance;
                  Val   : T;
                  Order : Mem_Order := Seq_Cst)
     with Post => Value (This) = Value (This)'Old + Val;

   procedure Sub (This  : aliased in out Instance;
                  Val   : T;
                  Order : Mem_Order := Seq_Cst)
     with Post => Value (This) = Value (This)'Old - Val;

   procedure Add_Fetch (This  : aliased in out Instance;
                        Val   : T;
                        Result : out T;
                        Order : Mem_Order := Seq_Cst)
     with Post => Result = (Value (This)'Old + Val)
     and then Value (This) = Result;

   procedure Sub_Fetch (This  : aliased in out Instance;
                       Val   : T;
                        Result : out T;
                        Order : Mem_Order := Seq_Cst)
     with Post => Result = (Value (This)'Old - Val)
     and then Value (This) = Result;

   procedure Fetch_Add (This  : aliased in out Instance;
                        Val   : T;
                        Result : out T;
                        Order : Mem_Order := Seq_Cst)
     with Post => Result = Value (This)'Old
     and Value (This) = (Value (This)'Old + Val);

   procedure Fetch_Sub (This  : aliased in out Instance;
                       Val   : T;
                        Result : out T;
                        Order : Mem_Order := Seq_Cst)
     with Post => Result = Value (This)'Old
     and Value (This) = (Value (This)'Old - Val);

   -- NOT SPARK compatible --

   function Exchange (This  : aliased in out Instance;
                      Val   : T;
                      Order : Mem_Order := Seq_Cst)
                      return T
     with SPARK_Mode => Off,
          Post => Exchange'Result = Value (This)'Old
     and then Value (This) = Val;

   function Compare_Exchange (This          : aliased in out Instance;
                              Expected      : T;
                              Desired       : T;
                              Weak          : Boolean;
                              Success_Order : Mem_Order := Seq_Cst;
                              Failure_Order : Mem_Order := Seq_Cst)
                              return Boolean
     with SPARK_Mode => Off,
          Post =>
       Compare_Exchange'Result = (Value (This)'Old = Expected)
     and then
       (if Compare_Exchange'Result then Value (This) = Desired);

   function Add_Fetch (This  : aliased in out Instance;
                       Val   : T;
                       Order : Mem_Order := Seq_Cst)
                       return T
     with SPARK_Mode => Off,
          Post => Add_Fetch'Result = (Value (This)'Old + Val)
     and then Value (This) = Add_Fetch'Result;

   function Sub_Fetch (This  : aliased in out Instance;
                       Val   : T;
                       Order : Mem_Order := Seq_Cst)
                       return T
     with SPARK_Mode => Off,
          Post => Sub_Fetch'Result = (Value (This)'Old - Val)
     and then Value (This) = Sub_Fetch'Result;

   function Fetch_Add (This  : aliased in out Instance;
                       Val   : T;
                       Order : Mem_Order := Seq_Cst)
                       return T
     with SPARK_Mode => Off;

   function Fetch_Sub (This  : aliased in out Instance;
                       Val   : T;
                       Order : Mem_Order := Seq_Cst)
                       return T
     with SPARK_Mode => Off;

private

   type Instance is new T;

   ----------
   -- Init --
   ----------

   function Init (Val : T) return Instance
   is (Instance (Val));

   -----------
   -- Value --
   -----------

   function Value (This : Instance) return T
   is (T (This));

   pragma Inline (Init);
   pragma Inline (Load);
   pragma Inline (Store);
   pragma Inline (Exchange);
   pragma Inline (Compare_Exchange);
   pragma Inline (Add);
   pragma Inline (Sub);
   pragma Inline (Add_Fetch);
   pragma Inline (Sub_Fetch);
   pragma Inline (Fetch_Add);
   pragma Inline (Fetch_Sub);

end Atomic.Generic_Signed32;
