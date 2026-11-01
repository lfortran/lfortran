program integer_arithmetic_nsw_do
implicit none
integer :: lo, hi, step
integer(1) :: lo1, hi1, step1
integer(1) :: i1
integer :: count
integer(8) :: lo8, hi8, step8
character(32) :: input

input = "-128 -125 1"
read(input, *) lo1, hi1, step1
lo1 = int(-huge(lo) / huge(lo) - 127, 1)
count = 0
do i1 = lo1, lo1 + 3_1
    count = count + 1
end do
if (count /= 4 .or. i1 /= -124_1) error stop
call check1(lo1, hi1, 1_1, 4)
call check1(lo1, hi1, step1, 4)
call check1(hi1, lo1, 1_1, 0)
call check1(lo1, hi1, -1_1, 0)
call check1(lo1, hi1, 2_1, 2)
lo1 = 127_1
hi1 = 124_1
call check1(lo1, hi1, -1_1, 4)
step1 = -1_1
call check1(lo1, hi1, step1, 4)
call check1(hi1, lo1, -1_1, 0)
call check1(lo1, hi1, -2_1, 2)

input = "-2147483648 -2147483645 1"
read(input, *) lo, hi, step
call check4(lo, hi, 1, 4)
call check4(lo, hi, step, 4)
call check4(hi, lo, 1, 0)
call check4(lo, hi, -1, 0)
lo = huge(lo)
hi = lo - 3
call check4(lo, hi, -1, 4)
step = -1
call check4(lo, hi, step, 4)
call check4(hi, lo, -1, 0)

input = "-9223372036854775808"
read(input, *) lo8
hi8 = lo8 + 3_8
step8 = 1_8
call check8(lo8, hi8, 1_8, 4)
call check8(lo8, hi8, step8, 4)
call check8(hi8, lo8, 1_8, 0)
call check8(lo8, hi8, -1_8, 0)
lo8 = huge(lo8)
hi8 = lo8 - 3_8
call check8(lo8, hi8, -1_8, 4)
step8 = -1_8
call check8(lo8, hi8, step8, 4)
call check8(hi8, lo8, -1_8, 0)
contains
subroutine check1(a, b, c, expected)
integer(1), intent(in) :: a, b, c
integer, intent(in) :: expected
integer(1) :: i
integer :: count
count = 0
do i = a, b, c
    count = count + 1
    if (count > 8) error stop
end do
if (count /= expected) error stop
if (int(i) /= int(a) + expected * int(c)) error stop
end subroutine
subroutine check4(a, b, c, expected)
integer, intent(in) :: a, b, c, expected
integer :: i, count
count = 0
do i = a, b, c
    count = count + 1
    if (count > 8) error stop
end do
if (count /= expected) error stop
if (int(i, 8) /= int(a, 8) + int(expected, 8) * int(c, 8)) error stop
end subroutine
subroutine check8(a, b, c, expected)
integer(8), intent(in) :: a, b, c
integer, intent(in) :: expected
integer(8) :: i
integer :: count
count = 0
do i = a, b, c
    count = count + 1
    if (count > 8) error stop
end do
if (count /= expected) error stop
if (i /= a + int(expected, 8) * c) error stop
end subroutine
end program
