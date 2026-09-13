! An allocatable component that is allocated but holds no elements in any
! element of the array it is handed over with. The component reaches the
! kernel as a flat buffer of every element's data laid end to end, which is
! then empty; the buffer still has to have a byte in it, because the launch
! takes the address of its first element. The loop runs on the device and
! the component that does have elements is read correctly.
module gpu_metal_321_mod
implicit none
type :: op_t
    real, allocatable :: upper_(:,:)
    real, allocatable :: inner_(:)
end type
contains
subroutine work(ops, r)
    type(op_t), intent(in) :: ops(2)
    real, intent(out) :: r(3)
    integer :: i
    do concurrent (i = 1:3)
        r(i) = real(i) + ops(1)%inner_(1) + ops(2)%inner_(2)
    end do
end subroutine
end module

program gpu_metal_321
use gpu_metal_321_mod
implicit none
type(op_t) :: ops(2)
real :: r(3)
integer :: i

do i = 1, 2
    allocate(ops(i)%upper_(0, 3))
    allocate(ops(i)%inner_(2))
    ops(i)%inner_ = [1.0, 2.0]
end do

call work(ops, r)

do i = 1, 3
    if (abs(r(i) - (real(i) + 3.0)) > 1.0e-5) error stop
end do
if (size(ops(1)%upper_) /= 0) error stop
if (abs(ops(2)%inner_(2) - 2.0) > 1.0e-5) error stop

print *, "PASS"
end program
