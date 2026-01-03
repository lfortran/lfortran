program implied_do_loops4
integer :: i, j
real :: x(5)
j = 12
x = (/ (real(i),i=1,5) /)
print *, (i,i=1,3), (i,i=4,6), j
write (*,*) (i,i=1,3), (i,i=4,6), j
print *, (x(i) + 1.0, i=1,5)
end program
