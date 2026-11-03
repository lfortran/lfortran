program gpu_metal_651
! integer(8) values in a kernel: arithmetic past huge(0), mixed with
! integer(4) and real(4), conversions both ways, comparisons and a scalar
! transfer between two integer(8) values, with a default integer loop index.
use iso_fortran_env, only: int32, int64, real32
implicit none
integer, parameter :: n = 64
integer(int64) :: base, lo, s(n), d(n), t(n)
integer(int32) :: k(n), back(n)
real(real32) :: r(n)
logical :: big(n), neg(n)
integer :: i
base = 5000000000_int64
lo = -huge(0_int64) - 1_int64
do concurrent (i = 1:n)
    s(i) = base + i
    d(i) = s(i) * 3_int64 - 4000000000_int64 * i
    k(i) = int(mod(d(i), 1000_int64), int32)
    r(i) = real(base / 1000000_int64, real32) + real(i, real32)
    back(i) = int(int(r(i) * 2.0, int64) - 10000_int64, int32)
    big(i) = d(i) > 7000000000_int64 .and. max(base, s(i)) >= base
    neg(i) = lo < 0_int64 .and. -s(i) <= -base
    t(i) = transfer(d(i), 0_int64) + transfer(-3000000000_int64 * i, lo)
end do
do i = 1, n
    if (s(i) /= 5000000000_int64 + i) error stop 1
    if (d(i) /= 15000000000_int64 + 3 * i - 4000000000_int64 * i) error stop 2
    if (k(i) /= int(mod(d(i), 1000_int64), int32)) error stop 3
    if (abs(r(i) - real(5000 + i, real32)) > 0.) error stop 4
    if (back(i) /= 2 * i) error stop 5
    if (big(i) .neqv. (d(i) > 7000000000_int64)) error stop 6
    if (.not. neg(i)) error stop 7
    if (t(i) /= d(i) - 3000000000_int64 * i) error stop 9
end do
if (count(big) /= 2) error stop 8
print *, s(n), d(1), k(n), r(n), back(n)
end program
