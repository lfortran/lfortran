program separate_compilation_59
use separate_compilation_59a
implicit none
real :: a(2), z(2)
a = make()
call fill(z)
print *, a, z
if (any(a /= 3.0)) error stop
if (any(z /= 42.0)) error stop
end program
