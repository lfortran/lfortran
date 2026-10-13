! Local arrays with a generic function call as the lower bound, the upper bound, or both.
module automatic_array_03_m
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
        integer :: a(g(n):10)
        integer :: b(2:g(n))
        integer :: c(g(n):g(n+1))
        a = 1
        b = 1
        c = 1
        if (lbound(a, 1) /= 6 .or. sum(a) /= 5) error stop 1
        if (lbound(b, 1) /= 2 .or. sum(b) /= 5) error stop 2
        if (lbound(c, 1) /= 6 .or. sum(c) /= 3) error stop 3
    end subroutine
end module
program automatic_array_03
    use automatic_array_03_m
    implicit none
    call s(3)
    print *, "ok"
end program
