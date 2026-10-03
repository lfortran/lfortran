program select_rank_31
   implicit none
   integer :: scalar_k
   integer :: vec_k(3)
   integer :: padding(3)

   scalar_k = 5
   call set_padding_dispatch(scalar_k, padding(1:1))
   if (padding(1) /= 6) error stop "scalar"

   vec_k = [10, 20, 30]
   call set_padding_dispatch(vec_k, padding)
   if (any(padding /= [11, 21, 31])) error stop "vec"

contains

   subroutine set_padding_one(pad, k)
      integer, intent(out) :: pad
      integer, intent(in)  :: k
      pad = k + 1
   end subroutine set_padding_one

   subroutine set_padding_dispatch(kernel_size, padding)
      integer, dimension(..), intent(in) :: kernel_size
      integer, intent(out) :: padding(:)
      integer :: i

      select rank(kernel_size)
      rank(0)
         do i = 1, size(padding)
            call set_padding_one(padding(i), kernel_size)
         end do
      rank(1)
         do i = 1, size(kernel_size)
            call set_padding_one(padding(i), kernel_size(i))
         end do
      end select
   end subroutine set_padding_dispatch
end program select_rank_31
