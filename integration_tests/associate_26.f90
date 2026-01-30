program associate_26
   implicit none

   type :: str
       integer :: data
       integer(1) :: a
   end type

   type(str), target :: e(8)

   integer :: i,j,array_sum,loop_sum
   integer, pointer :: dd(:)
   integer, parameter :: idx(8) = [(i,i=1,8)]

   ! Set data
   e%data = idx

   !dd=>idx    ! OK
   dd=>e%data ! ICE

   array_sum = sum(idx(dd))
   loop_sum  = 0
   do j=1,size(dd)
       loop_sum = loop_sum+idx(dd(j))
   end do

   print *, 'expected =',sum(idx)
   print *, 'array_sum=',array_sum
   print *, ' loop_sum=', loop_sum

   if (array_sum/=sum(idx)) error stop 'array sum result is wrong'
   if ( loop_sum/=sum(idx)) error stop 'loop sum result is wrong'
   stop 0

end program
