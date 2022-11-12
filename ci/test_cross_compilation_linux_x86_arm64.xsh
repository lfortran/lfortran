#!/usr/bin/env xonsh

$RAISE_SUBPROC_ERROR = True
trace on

# Needed to run dynamically linked binaries with qemu user space emulation
sudo cp /usr/aarch64-linux-gnu/lib/ld-linux-aarch64.so.1 /lib/ld-linux-aarch64.so.1

$LFORTRAN_CC="/usr/bin/aarch64-linux-gnu-gcc"
# Run some simple compilation tests, works everywhere:
./inst_aarch64/bin/lfortran --version
# Compile and link separately
./inst_aarch64/bin/lfortran --link-with-gcc --target arm64-linux -c examples/expr2.f90 -o expr2.o
./inst_aarch64/bin/lfortran --link-with-gcc --target arm64-linux -o expr2 expr2.o
qemu-aarch64-static ./expr2

# Compile C and Fortran
./inst_aarch64/bin/lfortran --link-with-gcc --target arm64-linux -c integration_tests/modules_15b.f90 -o modules_15b.o
./inst_aarch64/bin/lfortran --link-with-gcc --target arm64-linux -c integration_tests/modules_15.f90 -o modules_15.o

# Linux
/usr/bin/aarch64-linux-gnu-gcc -c integration_tests/modules_15c.c -o modules_15c.o

./inst_aarch64/bin/lfortran --link-with-gcc --target arm64-linux modules_15.o modules_15b.o modules_15c.o -o modules_15
qemu-aarch64-static ./modules_15


# Compile and link in one step
./inst_aarch64/bin/lfortran --link-with-gcc --target arm64-linux integration_tests/intrinsics_04s.f90 -o intrinsics_04s
qemu-aarch64-static ./intrinsics_04s

./inst_aarch64/bin/lfortran --link-with-gcc --target arm64-linux integration_tests/intrinsics_04.f90 -o intrinsics_04
qemu-aarch64-static ./intrinsics_04


# Run all tests (does not work on Windows yet):
# cmake --version
# if not $IS_WIN:
#     ./run_tests.py

#     cd integration_tests
#     mkdir build-lfortran-llvm
#     cd build-lfortran-llvm
#     $FC="../../src/bin/lfortran"
#     cmake -DLFORTRAN_BACKEND=llvm ..
#     make
#     ctest -L llvm
#     cd ..

#     ./run_tests.py -b llvm2
