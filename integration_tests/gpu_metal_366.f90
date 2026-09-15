program gpu_metal_366
! A function that changes a `value` dummy argument before it sizes the
! component of its result gives the component the size of the changed value,
! not of the actual argument. Every written component is allocated before
! the loop with the size the loop gives it, so bounds checking reports
! nothing, without --realloc-lhs-arrays.
implicit none
type tt
    real, allocatable :: v(:)
end type
real :: c(6)
type(tt) :: t(3)
integer :: i
c = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
allocate(t(1)%v(2), t(2)%v(3))
allocate(t(3)%v(5))
t(3)%v = -1.0
do concurrent (i = 1:2)
    t(i) = h(c, i)
end do
print *, size(t(1)%v), size(t(2)%v), size(t(3)%v)
if (size(t(1)%v) /= 2) error stop 1
if (size(t(2)%v) /= 3) error stop 2
if (any(t(1)%v /= c(1:2))) error stop 3
if (any(t(2)%v /= c(1:3))) error stop 4
if (size(t(3)%v) /= 5) error stop 5
if (any(t(3)%v /= -1.0)) error stop 6
print *, "ok"
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
