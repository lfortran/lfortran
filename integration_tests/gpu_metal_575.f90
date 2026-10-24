module gpu_metal_575_m
implicit none

type :: inner_t
    integer :: b
end type

type :: pt
    integer :: a
    integer :: c(3)
    type(inner_t) :: in
end type

! Derived-type variables of the module: one read only by a procedure the
! do concurrent loop calls, one read directly in a loop body.
type(pt) :: v0 = pt(4, [10, 20, 30], inner_t(6))
type(pt) :: w0 = pt(1, [2, 3, 4], inner_t(5))

contains

pure integer function g(i)
    integer, intent(in) :: i
    g = i + v0%a + v0%c(3) + v0%in%b
end function

end module

program gpu_metal_575
use gpu_metal_575_m
implicit none
integer :: r(4), s(4), i

do concurrent (i = 1:4)
    r(i) = g(i)
end do
do i = 1, 4
    if (r(i) /= i + 40) error stop
end do

do concurrent (i = 1:4)
    s(i) = i + w0%a + w0%c(2) + w0%in%b
end do
do i = 1, 4
    if (s(i) /= i + 9) error stop
end do

print *, r, s
end program
