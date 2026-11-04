program gpu_shift_01
! Right shifts of negative integers of every kind in a parallel loop:
! ishft with a negative shift and shiftr fill with zeros (a logical shift),
! shifta copies the sign bit (an arithmetic shift).
use iso_fortran_env, only: int8, int16, int32, int64
implicit none
integer, parameter :: n = 6
integer :: i
integer(int8) :: a1(n), l1(n), r1(n), s1(n)
integer(int16) :: a2(n), l2(n), r2(n), s2(n)
integer(int32) :: a4(n), l4(n), r4(n), s4(n)
integer(int64) :: a8(n), l8(n), r8(n), s8(n)

a1 = [-1_int8, -2_int8, -100_int8, -huge(1_int8) - 1_int8, 1_int8, 100_int8]
a2 = [-1_int16, -2_int16, -1000_int16, -huge(1_int16) - 1_int16, 1_int16, 1000_int16]
a4 = [-1, -5, -123456789, -huge(1) - 1, 1, 123456789]
a8 = [-1_int64, -5_int64, -1234567890123_int64, -huge(1_int64) - 1, &
    1_int64, 1234567890123_int64]

do concurrent (i = 1:n)
    l1(i) = ishft(a1(i), -3)
    r1(i) = shiftr(a1(i), 3)
    s1(i) = shifta(a1(i), 3)
    l2(i) = ishft(a2(i), -5)
    r2(i) = shiftr(a2(i), 5)
    s2(i) = shifta(a2(i), 5)
    l4(i) = ishft(a4(i), -17)
    r4(i) = shiftr(a4(i), 17)
    s4(i) = shifta(a4(i), 17)
    l8(i) = ishft(a8(i), -33)
    r8(i) = shiftr(a8(i), 33)
    s8(i) = shifta(a8(i), 33)
end do

do i = 1, n
    if (l1(i) /= ishft(a1(i), -3)) error stop "ishft int8"
    if (r1(i) /= shiftr(a1(i), 3)) error stop "shiftr int8"
    if (s1(i) /= shifta(a1(i), 3)) error stop "shifta int8"
    if (l2(i) /= ishft(a2(i), -5)) error stop "ishft int16"
    if (r2(i) /= shiftr(a2(i), 5)) error stop "shiftr int16"
    if (s2(i) /= shifta(a2(i), 5)) error stop "shifta int16"
    if (l4(i) /= ishft(a4(i), -17)) error stop "ishft int32"
    if (r4(i) /= shiftr(a4(i), 17)) error stop "shiftr int32"
    if (s4(i) /= shifta(a4(i), 17)) error stop "shifta int32"
    if (l8(i) /= ishft(a8(i), -33)) error stop "ishft int64"
    if (r8(i) /= shiftr(a8(i), 33)) error stop "shiftr int64"
    if (s8(i) /= shifta(a8(i), 33)) error stop "shifta int64"
end do
! The logical shift of -1 is the largest value with the top bits clear.
if (l1(1) /= 31) error stop "ishft(-1_int8, -3)"
if (l2(1) /= 2047) error stop "ishft(-1_int16, -5)"
if (l4(1) /= 32767) error stop "ishft(-1, -17)"
if (l8(1) /= 2147483647_int64) error stop "ishft(-1_int64, -33)"
if (s4(1) /= -1) error stop "shifta(-1, 17)"
print *, "ok"
end program
