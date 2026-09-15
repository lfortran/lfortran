program gpu_metal_372
! An allocatable component of a function result, sized at run time from the
! actual argument, assigned to an element of a struct array in an offloaded
! `do concurrent`: the host has to give each element its size before launch.
implicit none
type tt
    real, allocatable :: v(:)
end type
real :: c(5)
real, allocatable :: d(:)
type(tt) :: t(2), u(2), w(3)
integer :: i, n

c = [1.0, 2.0, 3.0, 4.0, 5.0]
n = 3

! A section whose extent is a variable.
do concurrent (i = 1:2)
    t(i) = f(c(1:n))
end do
do i = 1, 2
    print *, size(t(i)%v), t(i)%v
    if (size(t(i)%v) /= 3) error stop
    if (any(t(i)%v /= c(1:3))) error stop
end do

! An allocatable actual argument.
allocate(d(4))
d = [10.0, 20.0, 30.0, 40.0]
do concurrent (i = 1:2)
    u(i) = f(d)
end do
do i = 1, 2
    print *, size(u(i)%v), u(i)%v
    if (size(u(i)%v) /= 4) error stop
    if (any(u(i)%v /= d)) error stop
end do

! An extent that changes from one iteration to the next.
do concurrent (i = 1:3)
    w(i) = f(c(1:i+1))
end do
do i = 1, 3
    print *, size(w(i)%v), w(i)%v
    if (size(w(i)%v) /= i + 1) error stop
    if (any(w(i)%v /= c(1:i+1))) error stop
end do

contains

pure function f(x) result(r)
    real, intent(in) :: x(:)
    type(tt) :: r
    r%v = x
end function

end program
