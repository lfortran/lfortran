! The program of #15964: the index an EXIT leaves is the one it had, and a
! completed loop leaves it one step past the end.
program do_loop_16
implicit none
integer :: i,j
real(8) :: u,cumulative
real(8) :: weights(2)=[0.35d0,0.65d0]
integer :: expected(3) = [1, 2, 2]
do i=1,3
   u = real(i,8)/4
   cumulative = 0
   do j=1,1
      cumulative = cumulative+weights(j)
      if (u<cumulative) exit
   end do
   print *,u,j
   if (j /= expected(i)) error stop
end do
end program
