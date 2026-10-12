module arrays_reshape_49_constants
implicit none
real(8), parameter :: values(3,1) = reshape([1, -2, 1], [3,1])
real(8) :: saved(2,2) = reshape([1, 2, 3, 4], [2,2])
end module

module arrays_reshape_49_mod
use arrays_reshape_49_constants, only: values, saved
implicit none
contains
subroutine test()
real(8), allocatable :: a(:,:), b(:,:)
allocate(a(3,1), b(2,2))
a = values
if (size(a, 1) /= 3 .or. size(a, 2) /= 1) error stop
if (abs(a(1,1) - 1.0_8) > 1e-12_8) error stop
if (abs(a(2,1) + 2.0_8) > 1e-12_8) error stop
if (abs(a(3,1) - 1.0_8) > 1e-12_8) error stop
b = saved
if (abs(b(1,1) - 1.0_8) > 1e-12_8) error stop
if (abs(b(2,1) - 2.0_8) > 1e-12_8) error stop
if (abs(b(1,2) - 3.0_8) > 1e-12_8) error stop
if (abs(b(2,2) - 4.0_8) > 1e-12_8) error stop
end subroutine
end module

program arrays_reshape_49
use arrays_reshape_49_constants, only: values, saved
use arrays_reshape_49_mod, only: test
implicit none
call test()
if (abs(values(2,1) + 2.0_8) > 1e-12_8) error stop
if (abs(sum(saved) - 10.0_8) > 1e-12_8) error stop
print *, values
print *, saved
end program
