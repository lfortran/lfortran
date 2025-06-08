PROGRAM implied_do_loop1
INTEGER :: i, j
REAL, DIMENSION(2) :: A
A(1) = 12.9
A(2) = 12.9
print *, (i, A(i), i = 1, 2)
! write(11, *) (i, ( j, A(j), j = 1, 2), A(i), i = 1, 2)
print *, (i, ( j, A(j), j = 1, 2), A(i), i = 1, 2)
END PROGRAM implied_do_loop1
