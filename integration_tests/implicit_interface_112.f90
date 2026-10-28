! An EXTERNAL statement in a module procedure hides a generic interface
! that the host module accesses by use association.
module implicit_interface_112_gen
    implicit none
    interface f
        module procedure f_mod
    end interface
    interface s
        module procedure s_mod
    end interface
contains
    real function f_mod(x)
        real, intent(in) :: x
        f_mod = -x
    end function

    subroutine s_mod(x)
        real, intent(inout) :: x
        x = -x
    end subroutine
end module

module implicit_interface_112_m
    use implicit_interface_112_gen, only: f, s
    implicit none
contains
    real function g(x)
        real :: x
        external :: f
        real :: f
        g = f(x)
    end function

    real function h(x)
        real :: x
        real :: f
        external :: f
        h = f(x)
    end function

    real function k(x)
        real :: x, y
        external :: s
        y = x
        call s(y)
        k = y
    end function
end module

real function f(x)
    real :: x
    f = 2*x
end function

subroutine s(x)
    real :: x
    x = 3*x
end subroutine

program implicit_interface_112
    use implicit_interface_112_m
    implicit none
    print *, g(3.0), h(4.0), k(5.0)
    if (abs(g(3.0) - 6.0) > 1e-6) error stop
    if (abs(h(4.0) - 8.0) > 1e-6) error stop
    if (abs(k(5.0) - 15.0) > 1e-6) error stop
end program
