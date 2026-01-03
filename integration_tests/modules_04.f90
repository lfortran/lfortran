module modules_04_a
implicit none

contains

subroutine b()
print *, "b()"
end subroutine

end module

program modules_04
use, intrinsic :: iso_fortran_env
implicit none

call f()

contains

    subroutine f()
    use modules_04_a, only: b
    call b()
    end subroutine

    integer function g()
    use :: modules_04_a, only: b
    call b()
    g = 5
    end function

end
