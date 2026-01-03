program parameter_12
implicit none
integer :: i
real, dimension(3), parameter :: a = [(i, i = 1, 3)]
print *, a
if (abs(a(1) - 1.0) > 1e-8) error stop
if (abs(a(2) - 2.0) > 1e-8) error stop
if (abs(a(3) - 3.0) > 1e-8) error stop
end program
