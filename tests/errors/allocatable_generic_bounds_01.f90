module allocatable_generic_bounds_01_m
    implicit none
    interface g
        procedure dbl
    end interface
contains
    pure integer function dbl(n)
        integer, intent(in) :: n
        dbl = 2*n
    end function
    subroutine s(n)
        integer, intent(in) :: n
        integer, allocatable :: c(g(n):g(n+1))
    end subroutine
end module
