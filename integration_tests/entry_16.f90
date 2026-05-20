real function f(x)
    real, intent(in) :: x
    real :: g
    f = x**2
    return
    entry g(x)
    g = 3.0*x
    return
end function

program entry_16
    implicit none
    interface
        real function f(x)
            real, intent(in) :: x
        end function
        real function g(x)
            real, intent(in) :: x
        end function
    end interface
    real :: r

    r = f(2.0)
    if (abs(r - 4.0) > 1e-6) error stop
    r = g(3.0)
    if (abs(r - 9.0) > 1e-6) error stop
    print *, "PASS"
end program
