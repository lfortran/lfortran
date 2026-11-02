module mangle_underscore_04_m
    implicit none
    integer :: x = 7
    ! The global names of these interfaces are the mangled names of `x` and
    ! `t`. The program does not use them, so its unit drops them, while this
    ! unit keeps them: both units must still agree on the names of `x` and `t`.
    interface
        subroutine mangle_underscore_04_m_x(k)
            integer, intent(inout) :: k
        end subroutine
        subroutine mangle_underscore_04_m_t(k)
            integer, intent(inout) :: k
        end subroutine
    end interface
contains
    subroutine t(k)
        integer, intent(inout) :: k
        k = k + 100
    end subroutine

    subroutine drv(k)
        integer, intent(inout) :: k
        external :: mu04_add
        call mu04_add(k, 3)
    end subroutine
end module

subroutine mu04_add(k, n)
    integer, intent(inout) :: k
    integer, intent(in) :: n
    k = k + n
end subroutine
