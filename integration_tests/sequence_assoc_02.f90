! Actual arrays are sequence associated, in array element order, with
! explicit-shape dummies of a different rank.
program sequence_assoc_02
implicit none
integer :: a(2,3), b(6), i, j
do j = 1, 3
    do i = 1, 2
        a(i,j) = 10*i + j
    end do
end do
call flat(a)
do i = 1, 6
    b(i) = i
end do
call matrix(b)
contains
subroutine flat(x)
    integer, intent(in) :: x(6)
    print *, x(1), x(2), x(3)
    if (x(1) /= 11 .or. x(2) /= 21 .or. x(3) /= 12) error stop 1
    if (x(4) /= 22 .or. x(5) /= 13 .or. x(6) /= 23) error stop 2
end subroutine
subroutine matrix(x)
    integer, intent(in) :: x(2,3)
    if (x(1,1) /= 1 .or. x(2,1) /= 2 .or. x(1,2) /= 3) error stop 5
    if (x(2,2) /= 4 .or. x(1,3) /= 5 .or. x(2,3) /= 6) error stop 6
end subroutine
end program
