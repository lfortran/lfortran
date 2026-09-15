program gpu_metal_362
! Built with --realloc-lhs-arrays. The element the offloaded loops write is
! picked by a function call, so the host cannot tell which elements they
! write before they run. It then changes no allocated component: t(3)%v and
! u(3)%v, which the loops do not write, keep their size and data.
implicit none
type tt
    real, allocatable :: v(:)
end type
real :: c(5)
type(tt) :: t(4), u(4)
integer :: i, n

c = [1.0, 2.0, 3.0, 4.0, 5.0]
n = 3

allocate(t(3)%v(1))
t(3)%v = 9.0
do concurrent (i = 1:2)
    t(g(i)) = f(c(1:3))
end do
if (size(t(1)%v) /= 3 .or. any(t(1)%v /= c(1:3))) error stop 1
if (size(t(2)%v) /= 3 .or. any(t(2)%v /= c(1:3))) error stop 2
if (size(t(3)%v) /= 1 .or. t(3)%v(1) /= 9.0) error stop 3

allocate(u(3)%v(1))
u(3)%v = 8.0
do concurrent (i = 1:2)
    u(g(i)) = f(c(1:n))
end do
if (size(u(1)%v) /= 3 .or. any(u(1)%v /= c(1:3))) error stop 4
if (size(u(2)%v) /= 3 .or. any(u(2)%v /= c(1:3))) error stop 5
if (size(u(3)%v) /= 1 .or. u(3)%v(1) /= 8.0) error stop 6
print *, "ok"

contains

pure integer function g(j)
    integer, intent(in) :: j
    g = j
end function

pure function f(a) result(r)
    real, intent(in) :: a(:)
    type(tt) :: r
    r%v = a
end function

end program
