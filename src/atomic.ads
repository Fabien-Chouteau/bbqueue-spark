package Atomic
with Preelaborate
is

   type Mem_Order is
     (Relaxed, -- Implies no inter-thread ordering constraints

      Consume,
      --  This is currently implemented using the stronger __ATOMIC_ACQUIRE
      --  memory order because of a deficiency in C++11's semantics for
      --  memory_order_consume.

      Acquire,
      --  Creates an inter-thread happens-before constraint from the release (or
      --  stronger) semantic store to this acquire load. Can prevent hoisting of
      --  code to before the operation.

      Release,
      --  Creates an inter-thread happens-before constraint to acquire (or
      --  stronger) semantic loads that read from this release store. Can
      --  prevent sinking of code to after the operation.

      Acq_Rel, -- Combines the effects of both Acquire and Release

      Seq_Cst); -- Enforces total ordering with all other Seq_Cst operations

   ----------
   -- Flag --
   ----------

   type Flag is limited private;

   function Init (Val : Boolean) return Flag;
   --  Can be used to initialize an Atomic_Flag:
   --
   --  A : Atomic.Flag := Atomic.Init (0);

   function Set (This  : aliased Flag;
                 Order : Mem_Order := Seq_Cst)
                 return Boolean;

   function Test_And_Set (This  : aliased in out Flag;
                          Order : Mem_Order := Seq_Cst)
                          return Boolean;

   procedure Clear (This : aliased in out Flag;
                    Order : Mem_Order := Seq_Cst);

private

   type Flag is mod 2 ** 8
     with Size => 8;

   pragma Inline (Init);
   pragma Inline (Set);
   pragma Inline (Test_And_Set);
   pragma Inline (Clear);
end Atomic;
