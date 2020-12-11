--  with Ada.Text_IO; use Ada.Text_IO;

with Atomic; use Atomic;

package body BBqueue
with SPARK_Mode => On
is

   -----------
   -- Grant --
   -----------

   procedure Grant (This : in out Offsets_Only;
                    G    : in out Write_Grant;
                    Size : Count)
   is
      Read, Write, Start : Count;
      Max : constant Count := This.Size;
      Already_Inverted : Boolean;
      In_Progress : Boolean;

   begin

      Test_And_Set (This.Write_In_Progress, In_Progress, Acq_Rel);
      if In_Progress then
         G.Result := Grant_In_Progress;
         G.Slice  := Empty_Slice;
         return;
      end if;

      if Size = 0 then
         Clear (This.Write_In_Progress, Release);
         G.Result := Empty;
         G.Slice  := Empty_Slice;
         return;
      end if;

      if Size > This.Size then
         Clear (This.Write_In_Progress, Release);
         G.Result := Insufficient_Size;
         G.Slice  := Empty_Slice;
         return;
      end if;

      --  Writer component. Must never write to `read`,
      --  be careful writing to `load`
      Write := Atomic_Count.Load (This.Write, Acquire);
      Read := Atomic_Count.Load (This.Read, Acquire);
      Already_Inverted := Write < Read;

      if Already_Inverted then

         --  The original comparison is ((Write + Size) < Read), it is modified
         --  to avoid integer overflow.
         if Count'Last - Size >= Write
           and then
           (Write + Size) < Read
         then
            --  Inverted, room is still available
            Start := Write;
         else
            --  Inverted, no room is available
            Clear (This.Write_In_Progress, Release);
            G.Result := Insufficient_Size;
            G.Slice   := Empty_Slice;
            This.Granted_Write_Size := 0;

            return;
         end if;

      else

         --  The original comparison is ((Write + Size) <= Max), it is modified
         --  to avoid integer overflow.
         if Count'Last - Size >= Write
           and then
             (Write + Size) <= Max
         then
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
               G.Result := Insufficient_Size;
               G.Slice  := Empty_Slice;
               This.Granted_Write_Size := 0;
               return;
            end if;

         end if;
      end if;

      --  This is what we want to prove: the granted slice is in the writeable
      --  area.
      pragma Assert (Size /= 0);
      pragma Assert (In_Writable_Area (This, Start));
      pragma Assert (In_Writable_Area (This, Start + Size - 1));

      Atomic_Count.Store (This.Reserve, Start + Size, Release);

      This.Granted_Write_Size := Size;

      G.Result := Valid;
      G.Slice  := (Size, Start, Start + Size - 1);
   end Grant;

   ------------
   -- Commit --
   ------------

   procedure Commit (This   : in out Offsets_Only;
                     G      : in out Write_Grant;
                     Size   :        Count := Count'Last)
   is
      Used, Write, Last, New_Write : Count;
      Max : constant Count := This.Size;
      Len : constant Count := This.Granted_Write_Size;
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
      Used := Count'Min (Len, Size);
      Write := Atomic_Count.Load (This.Write, Acquire);

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

      --  Nothing granted anymore
      This.Granted_Write_Size := 0;

      G.Result := Empty;
      G.Slice  := Empty_Slice;

      --  Allow subsequent grants
      Clear (This.Write_In_Progress, Release);

   end Commit;

   ----------
   -- Read --
   ----------

   procedure Read (This : in out Offsets_Only;
                   G    : in out Read_Grant)
   is
      Read, Write, Last, Size : Count;
      In_Progress : Boolean;
   begin

      Test_And_Set (This.Read_In_Progress, In_Progress, Acq_Rel);
      if In_Progress then
         G.Result := Grant_In_Progress;
         G.Slice  := Empty_Slice;
         return;
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

      Size := (if Write < Read then Last - Read else Write - Read);

      if Size = 0 then
         Clear (This.Read_In_Progress);
         G.Result := Empty;
         G.Slice  := Empty_Slice;
         return;
      end if;

      --  This is what we want to prove: the granted slice is in the readable
      --  area.
      pragma Assert (Size /= 0);
      pragma Assert (In_Readable_Area (This, Read));
      pragma Assert (In_Readable_Area (This, Read + Size - 1));

      This.Granted_Read_Size := Size;

      G.Result := Valid;
      G.Slice  := (Size, Read, Read + Size - 1);
   end Read;

   -------------
   -- Release --
   -------------

   procedure Release (This : in out Offsets_Only;
                      G    : in out Read_Grant;
                      Size :        Count := Count'Last)
   is
      Used : Count;
   begin
      --  Saturate the grant commit
      Used := Count'Min (This.Granted_Read_Size, Size);

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

      --  Nothing granted anymore
      This.Granted_Read_Size := 0;

      G.Result := Empty;
      G.Slice  := Empty_Slice;

      --  Allow subsequent read
      Clear (This.Read_In_Progress, Release);
   end Release;

   --  -----------
   --  -- Print --
   --  -----------
   --
   --  procedure Print (This : Buffer) is
   --     procedure Print (Pos : Count; C : Character);
   --     procedure Print (Pos : Count; C : Character) is
   --     begin
   --        for Index in Count range This.Buf'First .. This.Buf'Last + 1 loop
   --           if Pos = Index then
   --              Put (C);
   --           else
   --              Put (' ');
   --           end if;
   --        end loop;
   --        New_Line;
   --     end Print;
   --     use type Interfaces.Unsigned_8;
   --  begin
   --     for Index in This.Buf'Range loop
   --        Put (Character'Val (Character'Pos ('0') + (This.Buf (Index) mod 10)));
   --     end loop;
   --     New_Line;
   --
   --     Print (Atomic_Count.Load (This.Write, Seq_Cst) + 1, 'W');
   --     Print (Atomic_Count.Load (This.Read, Seq_Cst) + 1, 'R');
   --     Print (Atomic_Count.Load (This.Reserve, Seq_Cst) + 1, 'G');
   --     Print (Atomic_Count.Load (This.Last, Seq_Cst) + 1, 'L');
   --  end Print;

end BBqueue;
