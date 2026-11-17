program integer_pow_negative_01
! A negative integer exponent gives 1/base**(-exponent) in integer
! division: 1 for a base of 1, 1 or -1 for a base of -1, 0 otherwise.
implicit none
integer :: e
integer(1) :: b1, e1
integer(2) :: b2, e2
integer(8) :: e8
e = -9
if ((-1)**e /= -1) error stop 1
e = -4
if ((-1)**e /= 1) error stop 2
e = -1
if (1**e /= 1) error stop 3
if (2**e /= 0) error stop 4
if ((-2)**e /= 0) error stop 5
e8 = -5
if ((-1_8)**e8 /= -1_8) error stop 6
e8 = -huge(0_8) - 1
if ((-1_8)**e8 /= 1_8) error stop 7
b1 = -1
e1 = -3
if (b1**e1 /= -1_1) error stop 8
b2 = -1
e2 = -2
if (b2**e2 /= 1_2) error stop 9
print *, 'ok'
end program
