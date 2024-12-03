program openmp_40
integer :: i, j, res=0
do concurrent ( i =1:5, j = 1:10 )
    res=res+2
end do 
print *,"res =",res
end program