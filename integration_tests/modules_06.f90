module modules_06_a
implicit none

contains

integer function b() result(r)
print *, "b()"
r = 5
end function

end module

program modules_06
use modules_06_a, only: b
implicit none

integer :: i

i = b()

print *, i

end
