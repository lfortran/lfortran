! MOVE_ALLOC between allocatable components inside the loop that writes
! their elements.
module tbaa_06_m
implicit none

type :: pair_t
    real, allocatable :: a(:), b(:)
end type

contains

subroutine rotate(s, n)
    type(pair_t), intent(inout) :: s
    integer, intent(in) :: n
    real, allocatable :: tmp(:)
    integer :: i
    do i = 1, n
        s%a(1) = s%a(1) + i
        call move_alloc(s%a, tmp)
        call move_alloc(s%b, s%a)
        call move_alloc(tmp, s%b)
        s%a(2) = s%a(2) + 10*i
    end do
end subroutine

end module

program tbaa_06
use tbaa_06_m
implicit none
type(pair_t) :: s
allocate(s%a(2), s%b(3))
s%a = 0
s%b = 100
call rotate(s, 3)
print *, s%a, s%b
if (size(s%a) /= 3 .or. size(s%b) /= 2) error stop 1
if (any(abs(s%a - [102.0, 140.0, 100.0]) > 1e-6)) error stop 2
if (any(abs(s%b - [4.0, 20.0]) > 1e-6)) error stop 3
print *, "ok"
end program
