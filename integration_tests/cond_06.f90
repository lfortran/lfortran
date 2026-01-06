! Test integer comparisons with mixed kinds
program cond_06
    implicit none
    integer(4) :: i4
    integer(8) :: i8
    integer :: count

    i4 = 5
    i8 = 5
    count = 0

    if (i8 == i4) count = count + 1
    if (i4 == i8) count = count + 1
    if (i8 == 5) count = count + 1
    if (i8 /= 10) count = count + 1

    i8 = 10
    i4 = 5
    if (i8 > i4) count = count + 1
    if (i4 < i8) count = count + 1
    if (i8 >= i4) count = count + 1
    if (i4 <= i8) count = count + 1

    if (count /= 8) error stop
end program
