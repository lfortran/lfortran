program implied_do_loop1
	integer :: a(10)=(/(j,j=1,10)/)
	print*, (a(j),j=1,10)
end program