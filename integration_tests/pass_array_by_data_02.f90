program main
implicit none
real :: data_x(3) = [1.0, 2.0, 3.0]
call find_fit(data_x, expr)
if ( abs(data_x(1) - 1.0) > 1e-16 ) error stop
if ( abs(data_x(2) - 2.0) > 1e-16 ) error stop
if ( abs(data_x(3) - 3.0) > 1e-16 ) error stop
print *, data_x
contains

subroutine find_fit(data_x, expr)
real, intent(in) :: data_x(:)
interface
    function expr() result(y)
        real :: y
    end function expr
end interface
end subroutine find_fit

function expr() result(y)
real :: y
end function expr

end program main
