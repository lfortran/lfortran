program intrinsics_336
	integer, parameter :: SIMPLE_FACTORS(*) = [3,5,7,11,13,17,19,23]
	integer :: n
	n = 1
	if(any(mod(n,SIMPLE_FACTORS)==0)) error stop
end program