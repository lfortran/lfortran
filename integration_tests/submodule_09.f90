module stdlib_error_submodule_09
implicit none

interface
    module subroutine error_stop(code)
        integer, intent(inout) :: code
    end subroutine error_stop
end interface

end module

submodule (stdlib_error_submodule_09) f18estop_submodule_09
implicit none

contains

    module procedure error_stop
        code = 3
    end procedure

end submodule f18estop_submodule_09

program submodule_09
    use stdlib_error_submodule_09
    implicit none

    integer :: tester = 1
    call error_stop(tester)

    print *, tester
    if (tester /= 3) error stop
end program