! gh2941
program intrinsics_89
implicit none

integer :: x

x = (2+3)*5
print *, x, sin(0.5)
if (abs(sin(0.5) - 0.479425550) > 1e-7) error stop

end program
