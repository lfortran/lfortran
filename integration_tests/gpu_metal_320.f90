! An allocatable component reached through another component. The device
! lays a placeholder out in such a component's field and reads its data from
! a flat buffer named after the argument the component hangs off, and only a
! component of an argument itself has one, so a kernel reading `self%op_%a`
! would read the placeholder. Both loops stay on the host -- the polymorphic
! dummy and the plain one alike -- and compute the right answer there.
module gpu_metal_320_mod
implicit none
type :: inner_t
    real, allocatable :: a(:)
end type
type :: base_t
    type(inner_t) :: op_
    real :: c
end type
contains
subroutine work_class(self, r)
    class(base_t), intent(in) :: self
    real, intent(out) :: r(3)
    integer :: i
    do concurrent (i = 1:3)
        r(i) = self%c + self%op_%a(i)
    end do
end subroutine
subroutine work_type(self, r)
    type(base_t), intent(in) :: self
    real, intent(out) :: r(3)
    integer :: i
    do concurrent (i = 1:3)
        r(i) = self%c + self%op_%a(i)
    end do
end subroutine
end module

program gpu_metal_320
use gpu_metal_320_mod
implicit none
type(base_t) :: b
real :: r(3)
integer :: i

allocate(b%op_%a(3))
b%op_%a = [1.0, 2.0, 3.0]
b%c = 10.0

call work_class(b, r)
do i = 1, 3
    if (abs(r(i) - (10.0 + real(i))) > 1.0e-5) error stop
end do

r = 0.0
call work_type(b, r)
do i = 1, 3
    if (abs(r(i) - (10.0 + real(i))) > 1.0e-5) error stop
end do

if (size(b%op_%a) /= 3) error stop
if (abs(b%op_%a(2) - 2.0) > 1.0e-5) error stop

print *, "PASS"
end program
