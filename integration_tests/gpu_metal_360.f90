program gpu_metal_360
! Built with --realloc-lhs-arrays. The offloaded loops write a component
! only under `if` tests, so the host allocates the component of just the
! elements the tests let the loop write: an element they leave out keeps
! its allocation status, its size and its data.
implicit none
type tt
    real, allocatable :: v(:)
end type
real :: c(6)
type(tt) :: w(4), t(6), u(6)
integer :: i, n

c = [1.0, 5.0, 3.0, 7.0, 2.0, 9.0]
n = 3

allocate(w(2)%v(1))
w(2)%v = 9.0
do concurrent (i = 1:4)
    if (mod(i, 2) == 1) w(i) = f(c(1:n))
end do
if (size(w(1)%v) /= 3 .or. any(w(1)%v /= c(1:3))) error stop 1
if (size(w(3)%v) /= 3 .or. any(w(3)%v /= c(1:3))) error stop 2
if (size(w(2)%v) /= 1 .or. w(2)%v(1) /= 9.0) error stop 3
if (allocated(w(4)%v)) error stop 4

allocate(t(1)%v(1))
t(1)%v = -1.0
allocate(u(2)%v(1))
u(2)%v = -2.0
do concurrent (i = 1:6)
    if (c(i) > 2.5) then
        t(i) = f(c(1:n))
    else
        if (i > 1) u(i) = f(c(1:i))
    end if
end do
if (size(t(1)%v) /= 1 .or. t(1)%v(1) /= -1.0) error stop 5
if (allocated(t(5)%v)) error stop 6
do i = 2, 6
    if (i == 5) cycle
    if (size(t(i)%v) /= 3 .or. any(t(i)%v /= c(1:3))) error stop 7
end do
if (size(u(5)%v) /= 5 .or. any(u(5)%v /= c(1:5))) error stop 8
if (size(u(2)%v) /= 1 .or. u(2)%v(1) /= -2.0) error stop 9
if (allocated(u(1)%v) .or. allocated(u(3)%v)) error stop 10
if (allocated(u(4)%v) .or. allocated(u(6)%v)) error stop 11
print *, "ok"

contains

pure function f(a) result(r)
    real, intent(in) :: a(:)
    type(tt) :: r
    r%v = a
end function

end program
