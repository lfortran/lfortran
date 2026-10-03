! A polymorphic argument whose component is a derived type holding
! allocatable parts. The launch cannot copy such a component out of the
! class container in one assignment -- this late in the pass order that
! reaches the backend as a block copy of the array descriptors, after which
! the copy and the argument own the same storage -- so it copies the parts
! the device reads through the struct one by one and leaves the allocatable
! ones alone: they reach the kernel as flat buffers of their own. The loop
! runs on the device and the argument's own storage is untouched.
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
        r(i) = self%x_min_(1) + self%ops_(1)%upper_(i, 1) &
            + self%ops_(2)%upper_(1, i)
    end do
end subroutine
end module

program gpu_metal_317
use gpu_metal_317_mod
implicit none
type(base_t) :: b
real :: r(3)
integer :: i, j

do j = 1, 2
    allocate(b%ops_(j)%upper_(3, 3))
    do i = 1, 3
        b%ops_(j)%upper_(i, :) = real(10*j + i)
    end do
end do
b%x_min_ = 5.0
call work(b, r)

do i = 1, 3
    if (abs(r(i) - (5.0 + real(10 + i) + 21.0)) > 1.0e-5) error stop
end do

! The argument's own storage must still be there, and singly owned.
if (abs(b%ops_(1)%upper_(3, 3) - 13.0) > 1.0e-5) error stop
if (abs(b%ops_(2)%upper_(1, 2) - 21.0) > 1.0e-5) error stop
if (size(b%ops_(1)%upper_) /= 9) error stop
if (size(b%ops_(2)%upper_) /= 9) error stop
if (abs(b%x_min_(2) - 5.0) > 1.0e-5) error stop

print *, "PASS"
end program
