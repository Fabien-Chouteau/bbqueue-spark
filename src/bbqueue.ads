with System;
with Interfaces;

private with Atomic.Unsigned_32;

package BBqueue
--  with Preelaborate
is

   type Result_Kind is (Success, Grant_In_Progress, Insufficient_Size, Empty);

   type Write_Grant (Result : Result_Kind := Empty) is private;
   type Read_Grant (Result : Result_Kind := Empty) is private;

   subtype Count is Interfaces.Unsigned_32;

   type Buffer (Size : Count) is limited private;

   -- Producer --
   function Grant (This : aliased in out Buffer;
                   Size : Count)
                   return Write_Grant;

   -- Consumer --
   function Read (This : aliased in out Buffer)
                  return Read_Grant;

   procedure Commit (G    : in out Write_Grant;
                     Size :        Count := Count'Last)
     with Pre => G.Result = Success;
   --  For partial commits,

   procedure Release (G    : in out Read_Grant;
                      Size :        Count := Count'Last)
     with Pre => G.Result = Success;

   function Empty_Grant return Write_Grant;
   function Empty_Grant return Read_Grant;

   --  Tmp Internals
   function Size (G : Write_Grant) return Count;
   function Size (G : Read_Grant) return Count;
   function Index (G : Write_Grant) return Count;
   function Index (G : Read_Grant) return Count;
   function Addr (G : Write_Grant) return System.Address;
   function Addr (G : Read_Grant) return System.Address;

   procedure Print (This : Buffer);

private
   use type Interfaces.Unsigned_32;

   package Atomic_Count renames Atomic.Unsigned_32;

   type Storage_Array is array (Count range <>) of Interfaces.Unsigned_8;

   type Buffer (Size : Count) is limited record
      Buffer : Storage_Array (1 .. Size) := (others => 0);

      Write : aliased Atomic_Count.Instance := Atomic_Count.Init (0);
      --  Where the next byte will be written

      Read  : aliased Atomic_Count.Instance := Atomic_Count.Init (0);
      --  Where the next byte will be read from

      Last  : aliased Atomic_Count.Instance := Atomic_Count.Init (0);
      --  Used in the inverted case to mark the end of the
      --  readable streak. Otherwise will == sizeof::<self.buf>().
      --  Writer is responsible for placing this at the correct
      --  place when entering an inverted condition, and Reader
      --  is responsible for moving it back to sizeof::<self.buf>()
      --  when exiting the inverted condition

      Reserve  : aliased Atomic_Count.Instance := Atomic_Count.Init (0);
      --  Used by the Writer to remember what bytes are currently
      --  allowed to be written to, but are not yet ready to be
      --  read from

      Read_In_Progress : aliased Atomic.Flag := Atomic.Init (False);
      --  Is there an active read grant?

      Write_In_Progress : aliased Atomic.Flag := Atomic.Init (False);
      --  Is there an active write grant?
   end record;

   type Acc is access all Buffer;

   type Write_Grant (Result : Result_Kind := Empty) is record
      case Result is
         when Success =>
            BBQ       : not null Acc;
            Size      : Count;
            Index     : Count;
            Addr      : System.Address;
         when others => null;
      end case;
   end record;

   type Read_Grant (Result : Result_Kind := Empty) is new Write_Grant (Result);

   function Empty_Grant return Write_Grant
   is (Result => Empty);

   overriding
   function Empty_Grant return Read_Grant
   is (Result => Empty);

   function Size (G : Write_Grant) return Count
   is (G.Size);
   overriding
   function Size (G : Read_Grant) return Count
   is (G.Size);
   function Index (G : Write_Grant) return Count
   is (G.Index);
   overriding
   function Index (G : Read_Grant) return Count
   is (G.Index);
   function Addr (G : Write_Grant) return System.Address
   is (G.Addr);
   overriding
   function Addr (G : Read_Grant) return System.Address
   is (G.Addr);

end BBqueue;
