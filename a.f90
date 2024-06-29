program main
	implicit none
    integer, dimension(3) :: source = [1, 2, 3]
    integer, dimension(3, 2) :: result
	print *, spread([1, 2, 3], dim=2, ncopies=2)
    result = spread([1, 2, 3], dim=2, ncopies=2)
    print *, result
	print *, spread(source, 2, 2)
	print *, spread([1, 2, 3, 4, 5], 1, 2)
	print *, spread([1, 2, 3, 4, 5], 2, 2)
	print *, spread([1.0, 2.0, 3.0], 1, 3)
	print *, spread([(1, 2), (2, 3)], 1, 2)
	print *, spread(['a', 'b', 'c'], 1, 3)
	print *, spread([.true., .false., .true.], 1, 2)
end program
