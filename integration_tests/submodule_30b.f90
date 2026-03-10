module submodule_30_mod
    implicit none

    interface
        module subroutine compute(x, y)
            integer, intent(in) :: x
            integer, intent(out) :: y
        end subroutine
    end interface
end module
