#!/bin/sh

echo CLIB
gcc -m32 -c runtime.c -o lib.o

echo ASM
nasm -f elf32 -o prog.o prog.s

echo LINK
gcc -m32 -o prog lib.o prog.o

echo RUN
./prog

mv lib.o lib.oz
mv prog.o lib.oz