with Ada.Text_IO; use Ada.Text_IO;

with Atomic; use Atomic;

package body BBqueue is

   ---------------------------
   -- Err_Insufficient_Size --
   ---------------------------

   function Err_Insufficient_Size return Write_Grant
   is (Write_Grant'(Result => Insufficient_Size));

   ---------------------------
   -- Err_Insufficient_Size --
   ---------------------------

   function Err_Insufficient_Size return Read_Grant
   is (Read_Grant'(Result => Insufficient_Size));

   ---------------------------
   -- Err_Grant_In_Progress --
   ---------------------------

   function Err_Grant_In_Progress return Write_Grant
   is (Write_Grant'(Result => Grant_In_Progress));

   ---------------------------
   -- Err_Grant_In_Progress --
   ---------------------------

   function Err_Grant_In_Progress return Read_Grant
   is (Read_Grant'(Result => Grant_In_Progress));

   -----------
   -- Grant --
   -----------

   function Grant
     (This : aliased in out Buffer;
      Size : Count)
      return Write_Grant
   is
      Read, Write, Start : Count;
      Max : constant Count := This.Buffer'Length;
      Already_Inverted : Boolean;
   begin

      if Test_And_Set (This.Write_In_Progress, Acq_Rel) then
         return Err_Grant_In_Progress;
      end if;

      --  Writer component. Must never write to `read`,
      --  be careful writing to `load`
      Write := Atomic_Count.Load (This.Write, Acquire);
      Read := Atomic_Count.Load (This.Read, Acquire);
      Already_Inverted := Write < Read;

      if Already_Inverted then
         if (Write + Size) < Read then
            --  Inverted, room is still available
            Start := Write;
         else
            --  Inverted, no room is available
            Clear (This.Write_In_Progress, Release);
            return Err_Insufficient_Size;
         end if;
      else
         if (Write + Size) <= Max then
            --  Non inverted condition
            Start := Write;
         else
            --  Not inverted, but need to go inverted

            --  NOTE: We check Size < Read, NOT <=, because
            --  write must never == read in an inverted condition, since
            --  we will then not be able to tell if we are inverted or not
            if Size < Read then
               Start := 0;
            else
               --  Inverted, no room is available
               Clear (This.Write_In_Progress, Release);
               return Err_Insufficient_Size;
            end if;
         end if;
      end if;

      Atomic_Count.Store (This.Reserve, Start + Size, Release);

      return (Result => Success,
              BBQ    => This'Unchecked_Access,
              Size   => Size,
              Index  => This.Buffer'First + Start,
              Addr   => This.Buffer (This.Buffer'First + Start)'Address);
   end Grant;

   ------------
   -- Commit --
   ------------

   procedure Commit (G    : in out Write_Grant;
                     Size :        Count := Count'Last)
   is
      This : Buffer renames G.BBQ.all;
      Used, Write, Last, New_Write, Len : Count;
      Max : constant Count := This.Buffer'Length;
   begin
      --  If there is no grant in progress, return early. This
      --  generally means we are dropping the grant within a
      --  wrapper structure
      if not Set (This.Write_In_Progress, Acquire) then
         return;
      end if;

      --  Writer component. Must never write to READ,
      --  be careful writing to LAST

      --  Saturate the grant commit
      Len :=  G.Size;
      Used := Count'Min (Len, Size);
      Write := Atomic_Count.Load (This.Write, Acquire);

      --  TODO ATOMIC
      --  atomic::fetch_sub(&inner.reserve, len - used, AcqRel);
      Atomic_Count.Sub (This.Reserve, Len - Used, Acq_Rel);

      Last := Atomic_Count.Load (This.Last, Acquire);
      New_Write := Atomic_Count.Load (This.Reserve, Acquire);

      if (New_Write < Write) and then (Write /= Max) then
         --  We have already wrapped, but we are skipping some bytes at the end
         --  of the ring. Mark `last` where the write pointer used to be to hold
         --  the line here
         Atomic_Count.Store (This.Last, Write, Release);
      elsif New_Write > Last then
         --  We're about to pass the last pointer, which was previously the
         --  artificial end of the ring. Now that we've passed it, we can
         --  "unlock" the section that was previously skipped.
         --
         --  Since new_write is strictly larger than last, it is safe to move
         --  this as the other thread will still be halted by the (about to be
         --  updated) write value
         Atomic_Count.Store (This.Last, Max, Release);
      end if;
      --  else: If new_write == last, either:
      --  * last == max, so no need to write, OR
      --  * If we write in the end chunk again, we'll update last to max next
      --    time
      --  * If we write to the start chunk in a wrap, we'll update last when we
      --      move write backwards

      --  Write must be updated AFTER last, otherwise read could think it was
      --  time to invert early!
      Atomic_Count.Store (This.Write, New_Write, Release);

      G := (Result => Empty);

      --  Allow subsequent grants
      Clear (This.Write_In_Progress, Release);
   end Commit;

   ----------
   -- Read --
   ----------

   function Read
     (This : aliased in out Buffer)
      return Read_Grant
   is
      Read, Write, Last, Size : Count;
   begin

      if Test_And_Set (This.Read_In_Progress, Acq_Rel) then
         return Err_Grant_In_Progress;
      end if;

      Write := Atomic_Count.Load (This.Write, Acquire);
      Read  := Atomic_Count.Load (This.Read, Acquire);
      Last  := Atomic_Count.Load (This.Last, Acquire);

      --  Resolve the inverted case or end of read
      if Read = Last and then Write < Read then
         Read := 0;
         --  This has some room for error, the other thread reads this
         --  Impact to Grant:
         --    Grant checks if read < write to see if inverted. If not
         --      inverted, but no space left, Grant will initiate an inversion,
         --      but will not trigger it
         --  Impact to Commit:
         --    Commit does not check read, but if Grant has started an
         --    inversion, grant could move Last to the prior write position
         --  MOVING READ BACKWARDS!
         Atomic_Count.Store (This.Read, 0, Release);
      end if;

      Size := (if Write < Read then Last else Write) - Read;

      if Size = 0 then
         Clear (This.Read_In_Progress);
         return Err_Insufficient_Size;
      end if;

      return (Result => Success,
              BBQ    => This'Unchecked_Access,
              Size   => Size,
              Index  => This.Buffer'First + Read,
              Addr   => This.Buffer (This.Buffer'First + Read)'Address);
   end Read;

   -------------
   -- Release --
   -------------

   procedure Release (G    : in out Read_Grant;
                      Size :        Count := Count'Last)
   is
      This : Buffer renames G.BBQ.all;
      Used : Count;
   begin
      --  Saturate the grant commit
      Used := Count'Min (G.Size, Size);

      --  If there is no grant in progress, return early. This
      --  generally means we are dropping the grant within a
      --  wrapper structure
      if not Set (This.Read_In_Progress, Acquire) then
         return;
      end if;

      --  This should always be checked by the public interfaces
      --  debug_assert! (used <= self.buf.len ());

      --  This should be fine, purely incrementing
      Atomic_Count.Add (This.Read, Used, Release);

      G := (Result => Empty);

      Clear (This.Read_In_Progress, Release);
   end Release;

   -----------
   -- Print --
   -----------

   procedure Print (This : Buffer) is
   begin
      for Elt of This.Buffer loop
         Put (Elt'Img);
      end loop;
      New_Line;
   end Print;

end BBqueue;
