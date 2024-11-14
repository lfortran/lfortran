program intrinsics_288
	implicit none

    integer, dimension(3) :: source1 = [1, 2, 3]
	real, dimension(3) :: source2 = [1.0, 2.0, 3.0]
	logical, dimension(3) :: source3 = [.true., .false., .true.]
	complex, dimension(3) :: source4 = [(1, 2), (2, 3), (3, 4)]
	character(len=1), dimension(3) :: source5 = ['a', 'b', 'c']
    integer, dimension(3) :: source = [1, 2, 3]
    integer, dimension(3, 2) :: result
	integer, dimension(2, 3) :: result1
	real, dimension(3, 2) :: result2
	logical, dimension(2, 3) :: result3
	complex, dimension(3, 2) :: result4
	integer :: a = 1
    integer, dimension(2) :: result5
	character :: ch = 'b'
    character, dimension(2) :: result6
	logical :: lo = .true.
    logical, dimension(2) :: result7
	real :: rl = 1.0
    real, dimension(2) :: result8

    result = spread(source, dim=2, ncopies=2)
    print *, result
    if (result(1, 1) /= 1 .or. result(1, 2) /= 1 .or. result(2, 1) /= 2 .or. &
        result(2, 2) /= 2 .or. result(3, 1) /= 3 .or. result(3, 2) /= 3) error stop

	print *, spread([1, 4, 5], 2, 2)
	result = spread([1, 4, 5], 2, 2)
	print *, result(1, 1)
	! Does not work yet

	! if (result(1, 1) /= 1 .or. result(1, 2) /= 1 .or. result(2, 1) /= 4 .or. &
	! 	result(2, 2) /= 4 .or. result(3, 1) /= 5 .or. result(3, 2) /= 5) error stop
	print *, spread([1, 2, 3, 4, 5], 1, 2)
	print *, spread([1.0, 2.0, 3.0], 1, 3)
	print *, spread([(1, 2), (2, 3)], 1, 2)
	print *, spread(['a', 'b', 'c'], 1, 3)
	print *, spread([.true., .false., .true.], 1, 2)

	print *, spread(source1, 1, 2)
	result1 = spread(source1, 1, 2)
	print *, result1
	print *, result1(1, 2)
	if (result1(1, 1) /= 1 .or. result1(1, 2) /= 2 .or. result1(1, 3) /= 3 .or. &
		result1(2, 1) /= 1 .or. result1(2, 2) /= 2 .or. result1(2, 3) /= 3) error stop
	print *, spread(source2, 2, 2)
	result2 = spread(source2, 2, 2)
	if (abs(result2(1, 1) - 1.0) > 1e-6 .or. abs(result2(1, 2) - 1.0) > 1e-6 .or. abs(result2(2, 1) - 2.0) > 1e-6 .or. &
		abs(result2(2, 2) - 2.0) > 1e-6 .or. abs(result2(3, 1) - 3.0) > 1e-6 .or. abs(result2(3, 2) - 3.0) > 1e-6) error stop
	print *, spread(source3, 1, 2)
	result3 = spread(source3, 1, 2)
	print *, result3(2, 1)
	if (result3(1, 1) .neqv. .true. .or. result3(1, 2) .neqv. .false. .or. result3(1, 3) .neqv. .true. .or. &
		result3(2, 1) .neqv. .false. .or. result3(2, 2) .neqv. .false. .or. result3(2, 3) .neqv. .true.) error stop
	print *, spread(source4, 2, 2)
	result4 = spread(source4, 2, 2)
	if (result4(1, 1) /= (1, 2) .or. result4(1, 2) /= (1, 2) .or. result4(2, 1) /= (2, 3) .or. &
		result4(2, 2) /= (2, 3) .or. result4(3, 1) /= (3, 4) .or. result4(3, 2) /= (3, 4)) error stop
	print *, spread(source5, 1, 3) ! Wrong output with LFortran

	! tests for Scalar Sources
	result5 = SPREAD(a, 1, 2)
    print *, result5
    if (result5(1) /= 1 .or. result5(2) /= 1 ) error stop

	result6 = SPREAD(ch, 1, 2)
    print *, result6
    if (result6(1) /= 'b' .or. result6(2) /= 'b' ) error stop

	result7 = SPREAD(lo, 1, 2)
    print *, result7
    if ( (result7(1) .neqv. .true.) .or. (result7(2) .neqv. .true.) )  error stop

	result8 = SPREAD(rl, 1, 2)
    print *, result8
    if ( abs(result8(1) - 1.0) > 1e-6 .or. abs(result8(2) - 1.0) > 1e-6 ) error stop

end program
