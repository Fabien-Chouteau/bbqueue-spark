# bbqueue-ada
An Ada/SPARK proved implementation of James Munns' BBQueue

`BBqueue` implements lock free, one producer one consumer, BipBuffers.

The root package `BBqueue` only handles index offsets without having an
internal buffer. It can be used to allocate slices of an existing array, e.g.:

```ada
   Buf : Storage_Array (8 .. 64) := (others => 0);
   Q   : aliased Offsets_Only (Buf'Length);
   WG  : Write_Grant := BBqueue.Empty;
   S   : Slice_Rec;
begin
   Grant (Q, WG, 8);
   if State (WG) = Valid then
      S := Slice (WG);
      Buf (Buf'First + S.From .. Buf'First + S.To) := (others => 42);
   end if;
   Commit (Q, WG);
```

The package `BBqueue.Buffers` is based on `BBqueue.Offsets_Only` and embeds an
internal buffer. It provides directly usable slices of memory from its internal
buffer:
```ada
   Q   : aliased Buffer (64);
   WG  : Write_Grant := Empty;
   S   : Slice_Rec;
begin
   Grant (Q, WG, 8);
   if State (WG) = Valid then
      declare
         B : Storage_Array (1 .. Slice (WG).Length)
           with Address => Slice (WG).Addr;
      begin
         B := (others => 42);
      end;
   end if;
   Commit (Q, WG);
```

The package `BBqueue.Buffers.Framed` is based on `BBqueue.Buffers` and uses
markers in the buffer to track the size of each commited write grants. The size
of consequent read grants will conrespond to the sizes of commited write
grants. It can be used to handle variable lenght packets:
```ada
   Q   : aliased Framed_Buffer (64);
   WG  : Write_Grant := Empty;
   RG  : Read_Grant := Empty;
   S   : Slice_Rec;
begin
   Grant (Q, WG, 8); -- Get a worst case grant of size 8
   Commit (Q, WG, 4); -- Only commit 4
   Grant (Q, WG, 8); -- Get a worst case grant of size 8
   Commit (Q, WG, 5); -- Only commit 5
   Read (W, RG); -- Returns a grant of size 4
```
