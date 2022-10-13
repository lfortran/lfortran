module modules_02_a
implicit none

contains

subroutine b()
print *, "b()"
end subroutine

end module

module modules_02_c
implicit none

contains

subroutine d()
print *, "d()"
end subroutine

subroutine e()
print *, "e()"
end subroutine

end module

program modules_02
use modules_02_a
use modules_02_c, only: x=>d
use modules_02_c, only: e
implicit none

call b()
call x()
call e()

end
