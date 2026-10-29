program arrays_146
implicit none
real :: a(6), b(3,4)
real :: actual(-2:1), matrix(-1:0,4:6), empty(5:4,9:11), empty_column(-1:0,4:3)
logical :: flags(2,3)
integer :: i,j
do i = -2, 1
    actual(i) = i
end do
call check_assumed(actual)
if (actual(-2) /= 11 .or. actual(1) /= 1) error stop 7
if (lbound(actual,1) /= -2 .or. ubound(actual,1) /= 1) error stop 8
do j = 4, 6
    do i = -1, 0
        matrix(i,j) = 10*i+j
    end do
end do
call check_assumed_rank2(matrix,-3,2,3)
if (matrix(-1,4) /= 31 .or. matrix(0,6) /= 6) error stop 9
if (lbound(matrix,1) /= -1 .or. ubound(matrix,1) /= 0) error stop 10
if (lbound(matrix,2) /= 4 .or. ubound(matrix,2) /= 6) error stop 11
call check_assumed_rank2(empty,-3,0,3)
call check_assumed_rank2(empty_column,-3,2,0)
call check(a,-2,3,1,-2,3)
call check(a,0,5,1,0,5)
call check(a,5,4,1,1,0)
call check_rank2(b,1)
call check_rank2(b,2)
call check_logical(flags,1)
call check_logical(flags,2)
contains
subroutine check_assumed(x)
real, intent(inout) :: x(:)
if (lbound(x,1,kind=8) /= 1_8) error stop 12
if (ubound(x,1,kind=8) /= 4_8) error stop 13
if (x(1) /= -2 .or. x(4) /= 1) error stop 14
x(1) = 11
end subroutine
subroutine check_assumed_rank2(x,lo,n1,n2)
integer, intent(in) :: lo,n1,n2
real, intent(inout) :: x(:,lo:)
integer :: d,expected_lo,expected_hi
do d = 1, 2
    expected_lo = 1
    expected_hi = n1
    if (d == 2) then
        expected_hi = 0
        if (n2 > 0) then
            expected_lo = lo
            expected_hi = lo+n2-1
        end if
    end if
    if (lbound(x,d,kind=8) /= expected_lo) error stop 15
    if (ubound(x,d,kind=8) /= expected_hi) error stop 16
end do
if (n1 > 0 .and. n2 > 0) then
    if (x(1,lo) /= -6 .or. x(n1,lo+n2-1) /= 6) error stop 17
    x(1,lo) = 31
end if
end subroutine
subroutine check(x,lo,hi,d,expected_lo,expected_hi)
integer, intent(in) :: lo,hi,d,expected_lo,expected_hi
real, intent(in) :: x(lo:hi)
if (lbound(x,d,kind=8) /= expected_lo) error stop 1
if (ubound(x,d,kind=8) /= expected_hi) error stop 2
end subroutine
subroutine check_rank2(x,d)
integer, intent(in) :: d
real, intent(in) :: x(-1:1,0:3)
if (lbound(x,d) /= d-2) error stop 3
if (ubound(x,d) /= 2*d-1) error stop 4
print *, lbound(x,d), ubound(x,d)
end subroutine
subroutine check_logical(x,d)
integer, intent(in) :: d
logical, intent(in) :: x(-1:0,3:5)
if (lbound(x,d) /= 4*d-5) error stop 5
if (ubound(x,d) /= 5*d-5) error stop 6
write(*,*) lbound(x,d), ubound(x,d)
end subroutine
end program
