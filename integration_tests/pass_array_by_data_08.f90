program pass_array_by_data_08
   implicit none
   integer, dimension(10) :: a

   interface
      subroutine fill_array(aa)
         integer, dimension(:), intent(out) :: aa
         integer :: i
      end subroutine fill_array
   end interface

   call fill_array(a)
   print *, a
   if( any(a /= [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) ) error stop
end program pass_array_by_data_08

subroutine fill_array(a)
   implicit none
   integer, dimension(:), intent(out) :: a
   integer :: i
   do i = 1, 10
      a(i) = i
   end do
end subroutine fill_array
