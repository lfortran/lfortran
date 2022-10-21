module modules_01_a
implicit none

contains

subroutine b()
print *, "b()"
end subroutine

end module

program modules_01
use modules_01_a, only: b
implicit none

call b()

end
