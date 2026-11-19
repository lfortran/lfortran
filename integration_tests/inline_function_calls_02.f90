! A call in the bounds of a DO loop is inlined under --fast before the loop:
! the loop over the elements of `x(g(i))%v` that array_op makes reads
! `g(i)` in its bounds and in its body, which share the expression.
program inline_function_calls_02
implicit none
type tt
    real, allocatable :: v(:)
end type
type(tt) :: x(3)
real :: c(3)
integer :: i
c = [1., 2., 3.]
do i = 1, 3
    allocate(x(i)%v(3))
    x(i)%v = 0
end do
do i = 1, 2
    x(g(i))%v = c*i
end do
if (any(x(1)%v /= c) .or. any(x(2)%v /= 2*c) .or. any(x(3)%v /= 0)) error stop 1
print *, "ok"
contains
pure integer function g(j)
    integer, intent(in) :: j
    g = j
end function
end program
