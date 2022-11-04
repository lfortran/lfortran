program implied_do_loop2

	integer, dimension(3,5) :: array
	array = reshape([(i,(i+j,j=1,4),i=1,3)], shape(array))

	print *, array

end program