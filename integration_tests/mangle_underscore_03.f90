module mangle_underscore_03_m
    implicit none
    interface
        integer function mu03_twice(n)
            integer, intent(in) :: n
        end function
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
end module

program mangle_underscore_03
    use mangle_underscore_03_m, only: drv
    implicit none
    integer :: k
    k = 2
    call drv(k)
    print '(I0)', k
    if (k /= 11) error stop
end program
