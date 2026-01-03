module memory_arrays_87
implicit none

contains

subroutine alloc_rvector_dp(x, n)
implicit none
integer, intent(in) :: n
real, allocatable, intent(out) :: x(:)

allocate(x(n))
x = 10

end subroutine alloc_rvector_dp
end module memory_arrays_87

module test_arrays_87
use memory_arrays_87, only: alloc_rvector_dp
implicit none

contains

subroutine call_allocate(x, n)
integer, intent(in) :: n
real, allocatable :: x(:)
integer :: r

call alloc_rvector_dp(x, n)
call xuse(r)
print *, r
if( r /= 100 ) error stop


contains

subroutine xuse(r)
implicit none

integer, intent(out) :: r
r = sum(x)

end subroutine

end subroutine

end module

program arrays_87
use test_arrays_87
implicit none

real, allocatable :: a(:)
call call_allocate(a, 10)

end program
