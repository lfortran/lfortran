module module_arrays_45
integer :: nx = 4
integer :: ny = 4
end module module_arrays_45

subroutine a(cs)
use module_arrays_45
real, dimension(nx), intent(in) :: cs
print *, cs
if (abs(sum(cs) - 10.0) > 1e-8) error stop
end subroutine

subroutine b(cs)
use module_arrays_45
real, dimension(nx, ny), intent(in) :: cs
print *, cs
end subroutine

program arrays_45
real, dimension(4) :: cs
real, dimension(4, 4) :: cs2
cs = [1.0, 2.0, 3.0, 4.0]
cs2 = 12.87
call a(cs)
call b(cs2)
end program
