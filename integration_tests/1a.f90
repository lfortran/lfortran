program loop_bound
implicit none
integer :: i,n
n = 3
do i=1,n
   n = n - 1 
   ! n has changed, but # of iterations is fixed at 3
   print*,"i,n=",i,n
end do
print*,"i,n=",i,n
end program loop_bound
! output from gfortran, g95, flang, and Intel Fortran:
!  i,n=           1           2
!  i,n=           2           1
!  i,n=           3           0
!  i,n=           4           0
! incorrect lfortran output:
! i,n= 1 2
! i,n= 2 1
! i,n= 2 1