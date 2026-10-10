program separate_compilation_56
! An elemental procedure imported from a separately compiled module, called
! with array actual arguments under --legacy-array-sections (#14094)
use separate_compilation_56a, only: f, s
implicit none
real :: a(3) = [1.0, 2.0, 3.0]
real :: b(3), c(3)

b = f(a)
print *, b
if (any(abs(b - [2.0, 4.0, 6.0]) > 1e-6)) error stop

print *, f(a(2:3))
if (any(abs(f(a(2:3)) - [4.0, 6.0]) > 1e-6)) error stop
if (abs(f(a(3)) - 6.0) > 1e-6) error stop

call s(a, c)
print *, c
if (any(abs(c - [2.0, 3.0, 4.0]) > 1e-6)) error stop
end program
