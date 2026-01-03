program functions_11
implicit none
    
    real :: x=5, y
    real :: p=5, q
    real :: a, b, c
    y = f(x)
    print *, y

    q = f_real(p)
    print *, q

contains

    double precision function f(a) result(b)
    real, intent(in) :: a
    real :: x
    x = 2
    b = a + f_real(0.0)
    end function
    
    double precision function f_real(a) result(b)
    real, intent(in) :: a
    if( a == 0.0 ) then
        b = 2.0
    else 
        b = a + f(1.0)
    end if
    end function

end program 
