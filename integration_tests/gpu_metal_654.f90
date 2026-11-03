program gpu_metal_654
! Integer powers in a kernel are exact in the kind of their operands: past
! 2**24, where a single precision pow() rounds, and past huge(0) for
! integer(8). A negative exponent of a base other than 1 or -1 gives 0.
use iso_fortran_env, only: int8, int16, int32, int64
implicit none
integer, parameter :: n = 8
integer(int32) :: p4(n), m4(n), e
integer(int64) :: p8(n), q8(n), e8
integer(int16) :: p2(n)
integer(int8) :: p1(n)
integer :: i
e = 19
e8 = 3
do concurrent (i = 1:n)
    p4(i) = 3 ** e + i ** 2
    m4(i) = 2 ** (-i) + (-3) ** (i - 4) + i
    p8(i) = (int(i, int64) * 1000_int64) ** e8
    q8(i) = 3_int64 ** (e8 * 13) - int(i, int64) ** 19
    p2(i) = int(i, int16) ** 4_int16
    p1(i) = 2_int8 ** int(i - 2, int8)
end do
do i = 1, n
    if (p4(i) /= 1162261467 + i * i) error stop 1
    if (i < 4 .and. m4(i) /= i) error stop 2
    if (i >= 4 .and. m4(i) /= (-3) ** (i - 4) + i) error stop 3
    if (p8(i) /= int(i, int64) ** 3 * 1000000000_int64) error stop 4
    if (q8(i) /= 4052555153018976267_int64 - int(i, int64) ** 19) error stop 5
    if (p2(i) /= i ** 4) error stop 6
    if (i >= 2 .and. p1(i) /= 2 ** (i - 2)) error stop 7
    if (i == 1 .and. p1(i) /= 0) error stop 8
end do
print *, p4(n), m4(1), p8(n), q8(n), p2(n), p1(n)
end program
