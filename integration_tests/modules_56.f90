module module_56
use ieee_arithmetic, only: ieee_is_nan

contains
logical function is_close_rsp(a) result(close)
    real, intent(in) :: a

    close = ieee_is_nan(a)
end function is_close_rsp
end module

program modules_56
use module_56, only: is_close_rsp
implicit none
real :: a
a = 0.0
print *, is_close_rsp(a)
if (is_close_rsp(a)) error stop
end program
