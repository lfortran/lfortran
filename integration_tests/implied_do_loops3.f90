program implied_do_loop3

	real:: a(3) = (/(j*3,j=1,3)/)
	real:: b(3) = (/(j*2,j=1,3)/)

	! implied do loop
	print *,"Loop 1"

	print *, ( i, ( i*j, j = 1, 3), i = 1, 3)

	! nested implied do loop (double)
	print *,"Loop 2"
	print*, ((a(i)*b(j), j=1, 2), i=1, 3)
	print *,"Loop 3"
	print*, ((a(i)*b(j), i=1, 3), j=1, 2)

	! nested implied do loop (triple)
	print *,"Loop4"
	print*, (i,(i+j,(i+j+k,k=1,4),j=1,2),i=1,3)
	print *,"Loop5"
	print*, (i,(i*j,((i*j)+k,k=1,4),j=1,2),i=1,3)

end program