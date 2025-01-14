program openmp_43
   implicit none

   integer :: i,sum_fun,sum_sub

   do concurrent (i = 1:4) reduce(+:sum_fun)
      sum_fun = sum_fun + FUN(i)
   end do
print *,"FUN sum->", sum_fun

   do concurrent (i = 1:4) reduce(+:sum_sub)
      call SUB(i)
      sum_sub = sum_sub + i
   end do

print *,"SUB sum->", sum_sub
if(sum_sub /= 30) error stop

if(sum_fun /= 30) error stop

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