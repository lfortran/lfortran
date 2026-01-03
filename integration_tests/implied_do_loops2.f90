program implied_do_loop2
	integer :: i, j
	integer, dimension(3,5) :: array
	array = reshape([(i,(i+j,j=1,4),i=1,3)], (/3,5/))
	print *, array

end program