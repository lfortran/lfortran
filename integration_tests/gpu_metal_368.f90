program gpu_metal_368
! A function that changes a `value` dummy argument before it sizes the
! component of its result, which is not allocated before the loop. The host
! cannot tell the size the changed value gives before the loop runs, so even
! with --realloc-lhs-arrays it does not allocate the component with the
! size of the actual argument: with bounds checking on, the launch stops
! and asks to allocate the component before the loop.
implicit none
type tt
    real, allocatable :: v(:)
end type
real :: c(6)
type(tt) :: t(2)
integer :: i
c = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
do concurrent (i = 1:2)
    t(i) = h(c, i)
end do
print *, size(t(1)%v), size(t(2)%v)
if (size(t(1)%v) /= 2 .or. size(t(2)%v) /= 3) error stop 1
if (any(t(2)%v /= c(1:3))) error stop 2
contains
pure function h(a, n) result(r)
    real, intent(in) :: a(:)
    integer, value :: n
    type(tt) :: r
    n = n + 1
    allocate(r%v(n))
    r%v = a(1:n)
end function
end program
