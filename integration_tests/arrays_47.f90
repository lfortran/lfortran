module module_arrays_47
integer :: nx = 4
integer :: ny = 4
integer :: nz = 4
contains

subroutine a(cs)
real, dimension(nx, ny, nz), intent(in) :: cs
print *, sum(cs)
if (abs(sum(cs) - 6343.99072) > 1e-8) error stop
end subroutine
end module module_arrays_47

program arrays_47
use module_arrays_47
real, dimension(4, 4, 4) :: cs
cs = 99.1248
call a(cs)
end program
