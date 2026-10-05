program real80_unary_minus_01
! Constant folding of the negation of a real(10) constant
implicit none
integer, parameter :: n = 3
real(10), parameter :: p = -2.5_10
real(10), parameter :: q = -(n * 2.0_10)
real(10), parameter :: r = -p
real(10) :: y

y = -2.5_10
if (y /= -2.5_10) error stop
if (y + 2.5_10 /= 0) error stop
if (p /= -2.5_10) error stop
if (p + 2.5_10 /= 0) error stop
if (q /= -6) error stop
if (r /= 2.5_10) error stop
print *, y, p, q, r
end program
