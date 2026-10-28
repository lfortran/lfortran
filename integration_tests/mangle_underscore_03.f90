module mangle_underscore_03_m
    implicit none
    interface
        integer function mu03_twice(n)
            integer, intent(in) :: n
        end function
        ! Its global name is the mangled name of the module procedure `t`
        subroutine mangle_underscore_03_m_t(k)
            integer, intent(inout) :: k
        end subroutine
    end interface
contains
    subroutine drv(k)
        integer, intent(inout) :: k
        call mu03_add(k, 3)
        k = mu03_twice(k)
        call inner(k)
    contains
        subroutine inner(j)
            integer, intent(inout) :: j
            external :: mu03_add
            call mu03_add(j, 1)
        end subroutine
    end subroutine

    subroutine t(k)
        integer, intent(inout) :: k
        k = k + 100
    end subroutine

    subroutine clash(k)
        integer, intent(inout) :: k
        ! Its global name is the mangled name of the local variable `x`
        external :: mangle_underscore_03_m_x
        integer :: x
        x = 5
        call mangle_underscore_03_m_x(k)
        call mangle_underscore_03_m_t(k)
        call t(k)
        k = k + x
    end subroutine
end module

program mangle_underscore_03
    use mangle_underscore_03_m, only: drv, clash
    implicit none
    integer :: k
    k = 2
    call drv(k)
    print '(I0)', k
    if (k /= 11) error stop
    k = 1
    call clash(k)
    print '(I0)', k
    if (k /= 1116) error stop
end program
