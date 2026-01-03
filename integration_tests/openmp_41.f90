program openmp_41
integer :: i, j, k, res=0, resi=0, resj=0, resk=0
print *,"----","i","j","k"
do concurrent (i = 1:4 ,j = 1:3 ,k = 1:2) reduce(+:res,resi,resj,resk)
    print *, "----" ,i,j,k
    res=res+1
    resi=resi+i
    resj=resj+j
    resk=resk+k
end do 
print *,"res = ",res
print *,"resi = ",resi
print *,"resj = ",resj
print *,"resk = ",resk

! Check the result
if ( resi /= 60 ) error stop ! (1+2+3+4) * 3 * 2=60
if ( resj /= 48 ) error stop ! (1+2+3) * 4 * 2  = 48
if ( resk /= 36 ) error stop ! (1+2) * 4* 3 = 36
if ( res /= 24 ) error stop  ! Total iterations = 4 * 3 * 2 = 24
end program