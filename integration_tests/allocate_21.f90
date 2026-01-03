module mod_allocate_21
contains

   subroutine molded_allocation(arr)
      real(8), intent(in)  :: arr(:,:)
      real(8), allocatable :: local_arr(:,:)

      allocate(local_arr, mold = arr)

      if (size(local_arr, 1) /= size(arr, 1) .or. &
          size(local_arr, 2) /= size(arr, 2)) error stop
   end subroutine molded_allocation

end module mod_allocate_21

program allocate_21
   use mod_allocate_21
   implicit none

   real(8), allocatable :: arr(:,:)
   integer :: i, j

   allocate(arr(2,3))
   
   call molded_allocation(arr)
end program allocate_21