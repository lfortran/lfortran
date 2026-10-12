! Generic interfaces declared in external procedures

subroutine apply(x)
    interface
        integer function twice(y)
            integer, intent(in) :: y
        end function
    end interface
    interface g
        procedure twice
    end interface
    integer, intent(inout) :: x
    x = g(x)
end subroutine

integer function twice(y)
    integer, intent(in) :: y
    twice = 2*y
end function

program generic_interface_external_procedure_01
    implicit none
    interface
        integer function scaled(n)
            integer, intent(in) :: n
        end function
    end interface
    integer :: v
    v = 5
    call apply(v)
    if (v /= 10) error stop
    if (scaled(3) /= 11) error stop
    print *, v, scaled(3)
end program

! An external procedure that follows the main program. Its generic `g` is not
! the one of `apply`, and is also referenced in a specification expression.
integer function scaled(n)
    implicit none
    interface g
        pure integer function triple(y)
            integer, intent(in) :: y
        end function
        procedure half
    end interface
    integer, intent(in) :: n
    integer :: a(g(n))
    a = 1
    scaled = sum(a) + int(g(4.0))
contains
    real function half(y)
        real, intent(in) :: y
        half = y/2
    end function
end function

pure integer function triple(y)
    integer, intent(in) :: y
    triple = 3*y
end function
