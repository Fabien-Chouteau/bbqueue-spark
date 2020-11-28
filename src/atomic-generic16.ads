generic
   type T is (<>);
package Atomic.Generic16
with Preelaborate
is
   --  Based on GCC atomic built-ins. See:
   --  https://gcc.gnu.org/onlinedocs/gcc/_005f_005fatomic-Builtins.html
   --
   --  The specification is exactly the same for all sizes of data (8, 16, 32,
   --  64).

   type Instance is limited private;
   --  This type is limited and private, it can only be manipulated using the
   --  primitives below.

   function Init (Val : T) return Instance;
   --  Can be used to initialize an atomic instance:
   --
   --  A : Atomic.Unsigned_8.Instance := Atomic.Unsigned_8.Init (0);

   function Load (This  : aliased Instance;
                  Order : Mem_Order := Seq_Cst)
                  return T;

   procedure Store (This  : aliased in out Instance;
                    Val   : T;
                    Order : Mem_Order := Seq_Cst);

   function Exchange (This  : aliased Instance;
                      Val   : T;
                      Order : Mem_Order := Seq_Cst)
                      return T;

   function Compare_Exchange (This          : aliased Instance;
                              Expected      : T;
                              Desired       : T;
                              Weak          : Boolean;
                              Success_Order : Mem_Order := Seq_Cst;
                              Failure_Order : Mem_Order := Seq_Cst)
                              return Boolean;

   procedure Add (This  : aliased Instance;
                  Val   : T;
                  Order : Mem_Order := Seq_Cst);
   procedure Sub (This  : aliased Instance;
                  Val   : T;
                  Order : Mem_Order := Seq_Cst);
   procedure Op_And (This  : aliased Instance;
                     Val   : T;
                     Order : Mem_Order := Seq_Cst);
   procedure Op_XOR (This  : aliased Instance;
                     Val   : T;
                     Order : Mem_Order := Seq_Cst);
   procedure Op_OR (This  : aliased Instance;
                    Val   : T;
                    Order : Mem_Order := Seq_Cst);
   procedure NAND (This  : aliased Instance;
                   Val   : T;
                   Order : Mem_Order := Seq_Cst);

   function Add_Fetch (This  : aliased Instance;
                       Val   : T;
                       Order : Mem_Order := Seq_Cst)
                       return T;
   function Sub_Fetch (This  : aliased Instance;
                       Val   : T;
                       Order : Mem_Order := Seq_Cst)
                       return T;
   function And_Fetch (This  : aliased Instance;
                       Val   : T;
                       Order : Mem_Order := Seq_Cst)
                       return T;
   function XOR_Fetch (This  : aliased Instance;
                       Val   : T;
                       Order : Mem_Order := Seq_Cst)
                       return T;
   function OR_Fetch (This  : aliased Instance;
                      Val   : T;
                      Order : Mem_Order := Seq_Cst)
                      return T;
   function NAND_Fetch (This  : aliased Instance;
                        Val   : T;
                        Order : Mem_Order := Seq_Cst)
                        return T;

   function Fetch_Add (This  : aliased Instance;
                       Val   : T;
                       Order : Mem_Order := Seq_Cst)
                       return T;
   function Fetch_Sub (This  : aliased Instance;
                       Val   : T;
                       Order : Mem_Order := Seq_Cst)
                       return T;
   function Fetch_And (This  : aliased Instance;
                       Val   : T;
                       Order : Mem_Order := Seq_Cst)
                       return T;
   function Fetch_XOR (This  : aliased Instance;
                       Val   : T;
                       Order : Mem_Order := Seq_Cst)
                       return T;
   function Fetch_OR (This  : aliased Instance;
                      Val   : T;
                      Order : Mem_Order := Seq_Cst)
                      return T;
   function Fetch_NAND (This  : aliased Instance;
                        Val   : T;
                        Order : Mem_Order := Seq_Cst)
                        return T;

private

   type Instance is new T;

   function Init (Val : T) return Instance
   is (Instance (Val));

   pragma Inline (Init);
   pragma Inline (Load);
   pragma Inline (Store);
   pragma Inline (Exchange);
   pragma Inline (Compare_Exchange);
   pragma Inline (Add);
   pragma Inline (Sub);
   pragma Inline (Op_And);
   pragma Inline (Op_XOR);
   pragma Inline (Op_OR);
   pragma Inline (NAND);
   pragma Inline (Add_Fetch);
   pragma Inline (Sub_Fetch);
   pragma Inline (And_Fetch);
   pragma Inline (XOR_Fetch);
   pragma Inline (OR_Fetch);
   pragma Inline (NAND_Fetch);
   pragma Inline (Fetch_Add);
   pragma Inline (Fetch_Sub);
   pragma Inline (Fetch_And);
   pragma Inline (Fetch_XOR);
   pragma Inline (Fetch_OR);
   pragma Inline (Fetch_NAND);

end Atomic.Generic16;
