program gpu_metal_652
! integer(8) array elements, an integer(8) offset added to a default integer
! subscript, and integer(8) reductions. Element offsets stay 32-bit.
use iso_fortran_env, only: int64
implicit none
integer, parameter :: n = 1000
integer(int64) :: q_offset, total, biggest, a(n), b(n)
real :: x(n + 10), y(n)
integer :: i
q_offset = 7_int64
do i = 1, n
    x(i) = real(i)
    a(i) = 3000000000_int64 + i
end do
x(n + 1:) = 0.
total = 0
biggest = -huge(0_int64) - 1_int64
do concurrent (i = 1:n) reduce(+:total) reduce(max:biggest)
    y(i) = x(q_offset + i - 7_int64) + real(a(i) - 3000000000_int64)
    b(i) = 2_int64 * a(i)
    total = total + a(i)
    biggest = max(biggest, a(i) * 4_int64)
end do
do i = 1, n
    if (y(i) /= real(2 * i)) error stop 1
    if (b(i) /= 6000000000_int64 + 2 * i) error stop 2
end do
if (total /= 3000000000_int64 * n + int(n, int64) * (n + 1) / 2) error stop 3
if (biggest /= 4_int64 * (3000000000_int64 + n)) error stop 4
print *, total, biggest
end program
