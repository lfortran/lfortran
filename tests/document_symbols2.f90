program expr2
use a, only: x, f
implicit none

x = (2+3)*5
print *, x
call f(x)

end program
