program openmp_43
   implicit none

   integer :: i

   do concurrent (i = 1:4)
      print *,"Fun->", FUN(i)
   end do
print *,"Fun->", FUN(2)
contains

   PURE integer function FUN(x)
      integer, intent(in) :: x
      FUN = x * x
   end function FUN

end program openmp_43