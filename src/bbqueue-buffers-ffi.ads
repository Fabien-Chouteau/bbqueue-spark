with Ada.Unchecked_Deallocation;

package BBqueue.Buffers.FFI is
   type BufferPtr is access BBqueue.Buffers.Buffer -- can't be "not null" bc. of deallocation
     with Convention => C;

   function Create (Size : Buffer_Size) return BufferPtr
     with Convention => C,
          Export => True,
          External_Name => "bbqueue_buffers_create";

   procedure Drop (Ptr : in out BufferPtr)
     with Convention => C,
          Export => True,
          External_name => "bbqueue_buffers_drop";

   procedure Free is
      new Ada.Unchecked_Deallocation (
         Buffer, BufferPtr);

end BBqueue.Buffers.FFI;
