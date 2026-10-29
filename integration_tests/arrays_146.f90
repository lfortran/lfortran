program arrays_146
implicit none
real :: a(6), b(3,4)
logical :: flags(2,3)
call check(a,-2,3,1,-2,3)
call check(a,0,5,1,0,5)
call check(a,5,4,1,1,0)
call check_rank2(b,1)
call check_rank2(b,2)
call check_logical(flags,1)
call check_logical(flags,2)
contains
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
