package Atomic
with Preelaborate,
     Spark_Mode
is

   type Mem_Order is
     (Relaxed, -- Implies no inter-thread ordering constraints

      Consume,
      --  This is currently implemented using the stronger __ATOMIC_ACQUIRE
      --  memory order because of a deficiency in C++11's semantics for
      --  memory_order_consume.

      Acquire,
      --  Creates an inter-thread happens-before constraint from the release
      --  (or stronger) semantic store to this acquire load. Can prevent
      --  hoisting of code to before the operation.

      Release,
      --  Creates an inter-thread happens-before constraint to acquire (or
      --  stronger) semantic loads that read from this release store. Can
      --  prevent sinking of code to after the operation.

      Acq_Rel, -- Combines the effects of both Acquire and Release

      Seq_Cst); -- Enforces total ordering with all other Seq_Cst operations

   for Mem_Order use
     (Relaxed => 0,
      Consume => 1,
      Acquire => 2,
      Release => 3,
      Acq_Rel => 4,
      Seq_Cst => 5);

   function Stronger (A, B : Mem_Order) return Boolean;

   ----------
   -- Flag --
   ----------

   type Flag is limited private;

   function Init (Val : Boolean) return Flag
     with Post => Value (Init'Result) = Val;
   --  Can be used to initialize an Atomic_Flag:
   --
   --  A : Atomic.Flag := Atomic.Init (0);

   function Set (This  : aliased Flag;
                 Order : Mem_Order := Seq_Cst)
                 return Boolean
     with Pre  => Order in Relaxed | Consume | Acquire | Seq_Cst,
          Post => Set'Result = Value (This);

   procedure Test_And_Set (This   : aliased in out Flag;
                           Result : out Boolean;
                           Order  : Mem_Order := Seq_Cst)
     with Post => Result = Value (This)'Old
     and then Value (This) = True;

   procedure Clear (This : aliased in out Flag;
                    Order : Mem_Order := Seq_Cst)
     with Pre  => Order in Relaxed | Release | Seq_Cst,
          Post => Value (This) = False;

   function Value (This : Flag) return Boolean
     with Ghost;
   --  Ghost function to get the value of an Flag without needing it aliased.
   --  This doesn't use the atomic built-ins.

private

   function Stronger (A, B : Mem_Order) return Boolean
   is (A > B);

   type Flag is mod 2 ** 8
     with Size => 8;

   -----------
   -- Value --
   -----------

   function Value (This : Flag) return Boolean
   is (This /= 0);

   pragma Inline (Init);
   pragma Inline (Set);
   pragma Inline (Test_And_Set);
   pragma Inline (Clear);
   pragma Inline (Value);

end Atomic;
