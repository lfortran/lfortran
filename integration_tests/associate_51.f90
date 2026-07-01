program associate_51
implicit none
real(8) :: a, b
a = -2.0_8
associate( x => a )
    b = 4.0_8 * abs(x)
end associate
if (abs(b - 8.0_8) > 1e-12_8) error stop
print *, "All tests passed"
end program
