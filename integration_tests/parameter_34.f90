program parameter_34
implicit none
real(8) :: b(2)
real(8), parameter :: w(2) = [1.0d0, 2.0d0]
b = -w
if (abs(b(1) + 1.0d0) > 1.0d-12) error stop 1
if (abs(b(2) + 2.0d0) > 1.0d-12) error stop 2
end program
