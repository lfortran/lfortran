program functions_01
implicit none

    integer :: x = 5, y
    real :: p = 5, q
    real :: a, b, c
    y = f(x)
    print *, y

    q = f_real(p)
    print *, q

    a = 20.0
    b = -30.0
    c = signr32(a, b)
    print *, c


contains

    integer function f(a) result(b)
    integer, intent(in) :: a
    integer :: x = 2
    b = a + x
    end function

    real function f_real(a) result(b)
    real, intent(in) :: a
    b = a + signr32(1.0, a)
    end function

    real(4) function signr32(x, y) result(r)
    real(4), intent(in) :: x, y
    r = x
    if ((x >= 0.0 .and. y >= 0.0) .or. (x <= 0.0 .and. y <= 0.0)) then
        r = x
    else
        r = -x
    end if
    end function

end program
