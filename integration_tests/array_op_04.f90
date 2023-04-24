module module_array_op_04
contains

subroutine find_fit(expr)
real :: y(1)
interface
    function expr() result(y)
        real :: y(1)
    end function expr
end interface
y = expr()
if (abs(y(1) - 1.0) > 1e-7) error stop
print *, y
end subroutine find_fit

end module module_array_op_04

program example_primes

use module_array_op_04, only: find_fit
implicit none

call find_fit(expression)

contains

function expression() result(y)
real :: y(1)
y = 1.0
end function expression

end program example_primes
