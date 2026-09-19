program gpu_metal_371
! The callee changes a value dummy argument through an associate name before
! it sizes the component, so the size is not the one the actual argument
! gives. Every written component is preallocated with the size the loop
! gives it (2 and 3), and the element the loop does not write keeps its data.
implicit none
type tt
    real, allocatable :: v(:)
end type
real :: c(6)
type(tt) :: t(3)
integer :: i
c = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
allocate(t(1)%v(2), t(2)%v(3))
allocate(t(3)%v(4))
t(3)%v = -1.0
do concurrent (i = 1:2)
    t(i) = h(c, i)
end do
print *, size(t(1)%v), size(t(2)%v), size(t(3)%v)
if (size(t(1)%v) /= 2 .or. size(t(2)%v) /= 3) error stop 1
if (any(t(1)%v /= c(1:2))) error stop 2
if (any(t(2)%v /= c(1:3))) error stop 3
if (size(t(3)%v) /= 4 .or. any(t(3)%v /= -1.0)) error stop 4
print *, "ok"
contains
pure function h(a, n) result(r)
    real, intent(in) :: a(:)
    integer, value :: n
    type(tt) :: r
    associate (m => n)
        m = m + 1
    end associate
    allocate(r%v(n))
    r%v = a(1:n)
end function
end program
