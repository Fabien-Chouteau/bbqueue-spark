alr build
gnatbind -n ./lib/bbqueue ./lib/bbqueue-buffers ./lib/bbqueue-buffers-ffi ./alire/cache/dependencies/atomic_0.4.1_14bb7db3/lib/

# this is broken, as I don't know how to build the harness
rustc -L lib -L /home/vscode/gnat/lib/gcc/x86_64-pc-linux-gnu/10.3.1/adalib -L /home/vscode/gnat/lib/gcc/x86_64-pc-linux-gnu/10.3.1/rts-native -L ./alire/cache/dependencies/atomic_0.4.1_14bb7db3/lib/ -L. -lgnat -lgnarl -lAtomic ./src/bbqueue-buffers-ffi.rs