program gpu_metal_650
! A read-only integer(int64) scalar the loop converts to real(real32) (#16604).
use iso_fortran_env, only: int64, real32
implicit none
integer(int64) :: q_offset
real(real32) :: x(2)
integer :: l
q_offset = 7_int64
do concurrent (l = 1:2)
    x(l) = real(q_offset, real32)
end do
if (any(abs(x - 7.) > 0.)) error stop
print *, x
end program
