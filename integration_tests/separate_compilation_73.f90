program separate_compilation_73
use separate_compilation_73a, only: c, half
implicit none
real(10) :: z
z = c
if (z /= 1.5_10) error stop
if (half(3.0_10) /= 1.5_10) error stop
end program
