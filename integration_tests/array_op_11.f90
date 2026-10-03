program array_op_11
! Test: parameter array multiplied by scalar should compile and run correctly.
! This verifies that the array_op pass correctly clears m_value on
! IntegerBinOp nodes after expanding array operations into element loops.
implicit none
integer, parameter :: x(2) = [1, 2]
integer :: y(2)
integer :: z(3)
integer, parameter :: a(3) = [10, 20, 30]

y = x * 2
if (y(1) /= 2) error stop
if (y(2) /= 4) error stop

y = x + 3
if (y(1) /= 4) error stop
if (y(2) /= 5) error stop

y = x - 1
if (y(1) /= 0) error stop
if (y(2) /= 1) error stop

z = a * 3
if (z(1) /= 30) error stop
if (z(2) /= 60) error stop
if (z(3) /= 90) error stop

z = a + a
if (z(1) /= 20) error stop
if (z(2) /= 40) error stop
if (z(3) /= 60) error stop

print *, "ok"
end program
