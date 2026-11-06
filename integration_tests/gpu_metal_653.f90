program gpu_metal_653
! An integer(8) loop index. CUDA numbers iterations in integer(8) and runs
! the loop; Metal numbers them in 32 bits and reports an error instead,
! which tests.toml checks.
use iso_fortran_env, only: int64
implicit none
integer(int64) :: i
integer(int64) :: x(10)
do concurrent (i = 1_int64:10_int64)
    x(i) = i * 1000000000_int64
end do
if (x(10) /= 10000000000_int64) error stop
print *, x(10)
end program
