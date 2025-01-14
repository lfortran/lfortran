program openmp_43
   implicit none

   integer :: i,fun_sum,sub_sum,x
   fun_sum = 0
   do concurrent (i = 1:4) reduce(+:fun_sum)
      fun_sum = fun_sum + FUN(i)
   end do

   print *,"FUN sum->", fun_sum
   sub_sum = 0
   do concurrent (i = 1:4) reduce(+:sub_sum)
      x=i
      call SUB(x)
      sub_sum = sub_sum + x
   end do

   print *,"SUB sum->", sub_sum
   if(sub_sum /= 30) error stop
   if(fun_sum /= 30) error stop

contains

   pure subroutine SUB(x)
      integer, intent(inout) :: x
       x = x * x
   end subroutine SUB

   PURE integer function FUN(x)
      integer, intent(in) :: x
      FUN = x * x
   end function FUN

end program openmp_43