! An allocatable component allocated, deallocated and reallocated inside
! the loop that writes its elements.
module tbaa_05_m
implicit none

type :: buf_t
    real, allocatable :: acc(:)
    real, allocatable :: grow(:)
end type

contains

subroutine regrow(self, n, total)
    type(buf_t), intent(inout) :: self
    integer, intent(in) :: n
    real, intent(out) :: total
    integer :: i, j
    total = 0
    do i = 1, n
        if (allocated(self%acc)) deallocate(self%acc)
        allocate(self%acc(i))
        do j = 1, i
            self%acc(j) = j*i
        end do
        total = total + sum(self%acc)
    end do
end subroutine

subroutine append(self, n)
    class(buf_t), intent(inout) :: self
    integer, intent(in) :: n
    real, allocatable :: tmp(:)
    integer :: i
    allocate(self%grow(0))
    do i = 1, n
        allocate(tmp(i))
        tmp(1:i-1) = self%grow
        tmp(i) = i
        call move_alloc(tmp, self%grow)
        self%grow(1) = self%grow(1) + self%grow(i)
    end do
end subroutine

end module

program tbaa_05
use tbaa_05_m
implicit none
type(buf_t) :: b
real :: total
call regrow(b, 4, total)
! sum over i of i*i*(i+1)/2 = 1 + 6 + 18 + 40
if (abs(total - 65) > 1e-6) error stop 1
if (size(b%acc) /= 4) error stop 2
if (abs(b%acc(4) - 16) > 1e-6) error stop 3
call append(b, 4)
print *, b%grow
if (any(abs(b%grow - [11.0, 2.0, 3.0, 4.0]) > 1e-6)) error stop 4
print *, "ok"
end program
