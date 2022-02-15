use std::num::NonZeroU32;
use std::ptr::NonNull;

#[repr(C)]
struct BBqueueBufferRaw {
    _priv: [u8; 0],
}

struct BBqueueBuffer {
    ptr: NonNull<BBqueueBufferRaw>,
}

impl BBqueueBuffer {
    fn new(size: NonZeroU32) -> BBqueueBuffer {
        let ptr = unsafe { bbqueue_buffers_create(size) };
        BBqueueBuffer { ptr: ptr }
    }
}

impl Drop for BBqueueBuffer {
    fn drop(&mut self) {
        // SAFETY:
        // todo
        unsafe { bbqueue_buffers_drop(self.ptr) };
    }
}

fn main() {
    unsafe { adainit() };
    let buffer = BBqueueBuffer::new(NonZeroU32::new(12).unwrap());

    drop(buffer);
    unsafe { adafinal() };
}

#[link(name="Bbqueue", kind="static")]
extern {
    fn bbqueue_buffers_create(size: NonZeroU32) -> NonNull<BBqueueBufferRaw>;
    fn bbqueue_buffers_drop(ptr: NonNull<BBqueueBufferRaw>); // ptr might become null, but is dead at that moment from the Rust side

}

#[link(name="adamain", kind="static")]
extern {
    fn adainit();
    fn adafinal();
}