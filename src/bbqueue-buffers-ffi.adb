package body BBqueue.Buffers.FFI is

   function Create (Size : Buffer_Size) return BufferPtr is
   begin
      return new BBqueue.Buffers.Buffer (Size);
   end Create;

   procedure Drop (Ptr : in out BufferPtr) is
   begin
      Free (Ptr);
   end Drop;

end BBqueue.Buffers.FFI;
