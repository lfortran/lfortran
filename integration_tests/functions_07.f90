module a
implicit none

contains

    real function f_a(u) result(v)
        real, intent(in) :: u
        v = u + 1.0
    end function

end module

module b
use a
implicit none

contains

    real function f_b(x) result(y)
        real, intent(in) :: x
        y = f_a(x) + 1.0
    end function

end module

module c
use b
implicit none

contains

    real function f_c(w) result(z)
        real, intent(in) :: w
        z = f_b(w) + 1.0
    end function

end module

program functions_07
use c
implicit none

    real :: p = 5, q

    q = f_c(p)
    print *, q

end program