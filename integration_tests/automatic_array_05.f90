! Local variables whose array bound or character length is an expression that calls a generic function.
module automatic_array_05_m
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
        integer :: a(g(n)+1:10)
        integer :: b(g(n)+1)
        character(len=g(n)+1) :: c
        a = 1
        b = 1
        if (lbound(a, 1) /= 7 .or. sum(a) /= 4) error stop 1
        if (lbound(b, 1) /= 1 .or. sum(b) /= 7) error stop 2
        if (len(c) /= 7) error stop 3
    end subroutine
end module
program automatic_array_05
    use automatic_array_05_m
    implicit none
    call s(3)
    print *, "ok"
end program
