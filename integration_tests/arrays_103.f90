module arrays_103_mod_1
   implicit none
   real(8), allocatable :: a(:,:)
end module arrays_103_mod_1


module arrays_103_mod_2
   use arrays_103_mod_1
   implicit none

contains

   subroutine automatic_array()
      implicit none
      real(8), dimension(size(a,1)) :: b
      integer :: i

      do i = 1, size(a,1)
         b(i) = sum(a(i,:))
      end do

      if (any(b /= [10.0d0, 20.0d0, 30.0d0])) error stop

   end subroutine automatic_array

end module arrays_103_mod_2


program arrays_103
   use arrays_103_mod_1
   use arrays_103_mod_2
   implicit none
   integer :: i, j

   allocate(a(3,4))

   do i = 1, 3
      do j = 1, 4
         a(i,j) = real(i*j, 8)
      end do
   end do

   call automatic_array()

end program arrays_103
