program intrinsics_213

    integer :: x4 = 5
	integer(8) :: x8 = 5
	real :: y4 = 5.02784
	real(8) :: y8 = 5.02784_8
	complex :: z4 = (5, 5.02784)
	complex(8) :: z8 = (5_8, 5.02784_8)
	
	print*, int(5)
	if (int(5) /= 5) error stop
    print*, int(5.02784)
	if (int(5.02784) /= 5) error stop
	print*, int((5, 5.02784))
	if (int((5, 5.02784)) /= 5) error stop

	print*, int(5, kind = 4)
	if (int(5, kind = 4) /= 5_4) error stop
    print*, int(5.02784, 8)
	if (int(5.02784, 8) /= 5_8) error stop
	print*, int((5, 5.02784), kind = 8)
	if (int((5, 5.02784), kind = 8) /= 5_8) error stop

	print*, int(x4)
	if (int(x4) /= 5) error stop
	print*, int(y4)
	if (int(y4) /= 5) error stop
	print*, int(z4)
	if (int(z4) /= 5) error stop

	print*, int(x4, kind = 8)
	if (int(x4, kind = 8) /= 5_8) error stop
	print*, int(y4, 4)
	if (int(y8, 4) /= 5) error stop
	print*, int(z4, 8)
	if (int(z4, 8) /= 5_8) error stop

	print*, int(x8)
	if (int(x8) /= 5) error stop
	print*, int(y8)
	if (int(y8) /= 5) error stop
	print*, int(z8)
	if (int(z8) /= 5) error stop
    
end program