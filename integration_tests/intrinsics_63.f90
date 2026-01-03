program intrinsics_63
implicit none

complex, allocatable :: a5(:, :)

allocate(a5(5, 10))
a5 = (5.0, 6.0)
print *, a5
if( any(a5 /= (5.0, 6.0)) ) error stop

end program
