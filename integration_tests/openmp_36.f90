program openmp_36
integer :: i, j, res = 0
do concurrent ( i =1:5, j = 1:3 )
    res = res + i * j
end do 
print *, res
end program