module submodule_29_mod
    implicit none
    integer, parameter :: multiplier = 5

    interface
        module subroutine greet(x, y)
            integer, intent(in) :: x
            integer, intent(out) :: y
        end subroutine
    end interface
end module
