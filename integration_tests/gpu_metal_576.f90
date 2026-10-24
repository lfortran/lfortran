module gpu_metal_576_m
implicit none

type :: inner_t
    integer :: b
end type

type :: pt
    integer :: a
    integer :: c(3)
    type(inner_t) :: in
end type

type, extends(pt) :: ept
    logical :: l
    real(8) :: d(2)
end type

! Derived-type named constants of the module, read only by procedures
! that the do concurrent loops call.
type(pt), parameter :: q0 = pt(7, [1, 2, 3], inner_t(5))
type(ept), parameter :: e0 = ept(2, [1, 1, 1], inner_t(3), .true., &
    [2.5d0, 4.0d0])

contains

pure integer function f(i)
    integer, intent(in) :: i
    f = i + q0%a + q0%c(2) + q0%in%b
end function

pure real(8) function h(i)
    integer, intent(in) :: i
    h = i + e0%a + e0%in%b + e0%d(2)
    if (.not. e0%l) h = -1
end function

end module

program gpu_metal_576
use gpu_metal_576_m
implicit none
! Derived-type named constants of the program used directly in the loop
! bodies.
type(pt), parameter :: p0 = pt(3, [4, 5, 6], inner_t(8))
type(ept), parameter :: x0 = ept(1, [0, 0, 9], inner_t(0), .false., &
    [0.5d0, 1.0d0])
integer :: o(4), r(4), i
real(8) :: t(4), u(4)

do concurrent (i = 1:4)
    o(i) = i + p0%a + p0%c(1) + p0%in%b
end do
do i = 1, 4
    if (o(i) /= i + 15) error stop
end do

do concurrent (i = 1:4)
    r(i) = f(i)
end do
do i = 1, 4
    if (r(i) /= i + 14) error stop
end do

! An extended type, with real and logical components.
do concurrent (i = 1:4)
    t(i) = h(i)
    u(i) = i + x0%a + x0%c(3) + x0%d(1)
    if (x0%l) u(i) = -1
end do
do i = 1, 4
    if (t(i) /= i + 9.0d0) error stop
    if (u(i) /= i + 10.5d0) error stop
end do

print *, o, r
print *, t, u
end program
