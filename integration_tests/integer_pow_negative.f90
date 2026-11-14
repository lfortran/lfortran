program integer_pow_negative
implicit none
integer :: e
integer(8) :: e8
e = -9
if ((-1)**e /= -1) error stop
e = -4
if ((-1)**e /= 1) error stop
e = -1
if (1**e /= 1) error stop
if (2**e /= 0) error stop
e8 = -5
if ((-1_8)**e8 /= -1_8) error stop
print *, 'ok'
end program
