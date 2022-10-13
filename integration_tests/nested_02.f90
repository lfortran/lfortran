module nested_02_a
implicit none

contains

subroutine b()
print *, "b()"
call c()
contains
    subroutine c()
    print *, 5
    end subroutine c
end subroutine b

end module

program nested_02
use nested_02_a, only: b
implicit none

call b()

end
