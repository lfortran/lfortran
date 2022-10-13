module function_08_a
implicit none

end module

program functions_08
implicit none

    real :: x = 5, y
    real :: p = 5, q
    real :: a, b, c
    y = f(x)
    print *, y

    q = f_real(p)
    print *, q

contains

    real function f(a) result(b)
    real, intent(in) :: a
    real :: x = 2
    b = a + f_real(0.0)
    end function

    real function f_real(a) result(b)
    real, intent(in) :: a
    if( a == 0.0 ) then
        b = 2.0
    else
        b = a + f(1.0)
    end if
    end function

end program
