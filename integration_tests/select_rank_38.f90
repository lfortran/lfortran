module select_rank_38_mod
   implicit none

   type :: arr_t
      real, dimension(:,:), allocatable :: val
   end type arr_t

contains

   subroutine extract_array(this, output)
      class(arr_t), intent(in) :: this
      real, allocatable, dimension(..), intent(out) :: output

      select rank(output)
      rank(2)
         output = this%val
      end select
   end subroutine extract_array

end module select_rank_38_mod


program select_rank_38
   use select_rank_38_mod
   implicit none
   type(arr_t) :: a
   real, allocatable, dimension(:,:) :: output_2d
   integer :: i, k

   do k = 1, 4
      allocate(a%val(6, 2))
      do i = 1, 6
         a%val(i, 1) = real(i) + real(k * 100)
         a%val(i, 2) = real(i) + 10.0
      end do

      call extract_array(a, output_2d)
      if (any(shape(output_2d) /= [6, 2])) error stop
      if (abs(output_2d(1,1) - real(1 + k * 100)) > 1.0e-6) error stop
      if (abs(output_2d(6,1) - real(6 + k * 100)) > 1.0e-6) error stop
      if (abs(output_2d(1,2) - 11.0) > 1.0e-6) error stop

      deallocate(output_2d)
      deallocate(a%val)
   end do
end program select_rank_38
