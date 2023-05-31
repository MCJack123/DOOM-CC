# DOOM in ComputerCraft
Port of DOOM for ComputerCraft, using a RISC-V hypervisor. Requires CraftOS-PC Accelerated with FFI enabled.

## Compiling
Requires a RISC-V GCC toolchain with newlib. The makefile assumes Arch Linux `riscv64-elf-gcc` + `riscv32-elf-newlib` - if on other systems, adjust the paths in the makefile.

To build, simply run `make`.

## Usage
This will only run on CraftOS-PC Accelerated with FFI enabled. If the program errors, you need to enable FFI:
1. Close *all* instances of CraftOS-PC
2. Open `config/global.json` in the CraftOS-PC data directory ([see here for the location](https://www.craftos-pc.cc/docs/saves))
3. Find `"jit_ffi_enable": false`, and change `false` to `true`
4. Save, exit and restart CraftOS-PC Accelerated

Make sure a DOOM WAD is present in the root of the computer, **and that its name is all lowercase.** Then run `riscvcc` to start.

## Theory
My first attempts were to translate the source code to Lua or another lanugage, but I quickly realized this would take much longer than I wanted to spend to get working. This port uses a 32-bit RISC-V emulator to run the original C DOOM code.

The emulator implements a select number of system calls to access the screen and filesystem, which are sent through ECALL instructions. The rest of the functionality is supplied by newlib and the DOOM code. The system has 32 MB of RAM to work with, which is plenty for DOOM to be able to run. On top of that, there's an extra 64k of memory-mapped I/O for the display and program arguments. It also integrates a very basic ELF loader to load the DOOM executable into memory.

RISC-V was chosen as the architecture because of its small number of instructions and simple instruction encoding. Because Lua (5.1) can't handle 64-bit integers with full precision, I chose to use 32-bit RISC-V instead. For performance, I added the M extension to allow "hardware" multiplication and division, which are pretty common in DOOM. However, floating-point math is omitted since DOOM uses fixed-point calculations, and it would make the instruction set much more complicated.

## License
The DOOM code and executables are licensed under the DOOM Source License in `DOOMLIC.TXT`. The RISC-V emulator in `riscvcc.lua` is licensed under the GPLv2 license.