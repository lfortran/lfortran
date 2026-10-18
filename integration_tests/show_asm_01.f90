program show_asm_01
! Uses intrinsics that the ASR passes lower before LLVM code generation.
! `src/lfortran/tests/CMakeLists.txt` also compiles it with `--show-asm`.
implicit none
integer :: n, a(3)
real :: x, y
n = command_argument_count()
if (n /= 0) error stop
a = [1, 2, 3]
if (sum(a) /= 6) error stop
x = 0.5
y = sin(x)
if (abs(y - 0.47942554) > 1e-6) error stop
print *, n, sum(a), y
end program
