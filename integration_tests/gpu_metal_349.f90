program gpu_metal_349
! A device function whose derived-type result has an allocatable component
! sized from an assumed-shape dummy bound to an array section. The host has
! to allocate each element's component before launch, from the extent of the
! section, and a row section has to reach the device function with its
! stride.
implicit none
type tt
    real, allocatable :: v(:)
end type
real :: c(3), c2(3, 4)
type(tt) :: t(2), u(3)
integer :: i, j

c = [1.0, 2.0, 3.0]
do i = 1, 3
    do j = 1, 4
        c2(i, j) = 10 * i + j
    end do
end do

do concurrent (i = 1:2)
    t(i) = f(c(:))
end do
do i = 1, 2
    print *, t(i)%v
    if (size(t(i)%v) /= 3) error stop
    if (any(t(i)%v /= c)) error stop
end do

do concurrent (i = 1:3)
    u(i) = f(c2(i, :))
end do
do i = 1, 3
    print *, u(i)%v
    if (size(u(i)%v) /= 4) error stop
    if (any(u(i)%v /= c2(i, :))) error stop
end do

contains

pure function f(x) result(r)
    real, intent(in) :: x(:)
    type(tt) :: r
    r%v = x
end function

end program
