program gpu_metal_185
! Test: function returning struct with allocatable array member
! called inside do concurrent with an array expression argument.
implicit none
type :: t_t
    real, allocatable :: v(:)
end type
type(t_t) :: a(2)
real :: x(2, 2)
integer :: i

x = 1.0

do concurrent(i=1:2)
    a(i) = f(x(:,i) + 1.0)
end do

if (size(a(1)%v) /= 2) error stop
if (size(a(2)%v) /= 2) error stop
if (abs(a(1)%v(1) - 2.0) > 1e-6) error stop
if (abs(a(1)%v(2) - 2.0) > 1e-6) error stop
if (abs(a(2)%v(1) - 2.0) > 1e-6) error stop
if (abs(a(2)%v(2) - 2.0) > 1e-6) error stop
print *, "PASSED"
contains
pure function f(v) result(r)
    real, intent(in) :: v(:)
    type(t_t) :: r
    r%v = v
end function
end program
