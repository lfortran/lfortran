program openmp_40
integer :: i, j
do concurrent ( i =1:4, j = 1:3 )
    print *,i
end do 
end program