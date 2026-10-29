program arrays_137
implicit none
real :: empty(5:4), nonempty(-2:3), matrix(-1:1,0:3)
integer :: d
if (lbound(empty,1) /= 1) error stop 1
if (ubound(empty,1) /= 0) error stop 2
do d = 1, 1
    if (lbound(empty,d,kind=8) /= 1_8) error stop 3
    if (ubound(empty,d,kind=8) /= 0_8) error stop 4
    if (lbound(nonempty,d) /= -2) error stop 5
    if (ubound(nonempty,d) /= 3) error stop 6
end do
do d = 1, 2
    if (lbound(matrix,d) /= d-2) error stop 7
    if (ubound(matrix,d) /= 2*d-1) error stop 8
    if (lbound(matrix,d-1+1) /= d-2) error stop 9
end do
end program
