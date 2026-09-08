! A polymorphic argument whose component is a derived type holding
! allocatable parts cannot be copied out of the class container: the copy
! the launch would make shares the argument's own storage. The loop stays
! on the host and the values are still the ones the host computed.
module gpu_metal_317_mod
implicit none
type :: op_t
    real, allocatable :: upper_(:,:)
end type
type :: base_t
    type(op_t) :: ops_(2)
    real :: x_min_(2)
end type
contains
subroutine work(self, r)
    class(base_t), intent(in) :: self
    real, intent(out) :: r(3)
    integer :: i
    do concurrent (i = 1:3)
        r(i) = self%x_min_(1) + real(i)
    end do
end subroutine
end module

program gpu_metal_317
use gpu_metal_317_mod
implicit none
type(base_t) :: b
real :: r(3)
integer :: i

do i = 1, 2
    allocate(b%ops_(i)%upper_(2, 2))
    b%ops_(i)%upper_ = real(i)
end do
b%x_min_ = 5.0
call work(b, r)

do i = 1, 3
    if (abs(r(i) - (5.0 + real(i))) > 1.0e-5) error stop
end do
if (abs(b%ops_(1)%upper_(2, 2) - 1.0) > 1.0e-5) error stop
if (abs(b%ops_(2)%upper_(1, 2) - 2.0) > 1.0e-5) error stop

print *, "PASS"
end program
