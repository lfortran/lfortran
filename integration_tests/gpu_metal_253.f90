module gpu_metal_253_mod
   implicit none

   type :: stencil_t
      real, allocatable :: weights_(:,:)
   end type

contains

   subroutine apply(s, res, n)
      !! The BLOCK-local workspace `w` is sized by the extent of an
      !! allocatable array component of the derived-type kernel argument
      !! `s`.  The host has to work that extent out before it launches
      !! the kernel, so `size(s%weights_, 1)` becomes a scalar kernel
      !! argument instead of being asked of the flat device buffer.
      type(stencil_t), intent(in) :: s
      real, intent(out) :: res(:)
      integer, intent(in) :: n
      integer :: j
      do concurrent (j = 1:n)
         block
            real :: w(size(s%weights_, 1))
            integer :: k
            do k = 1, size(w)
               w(k) = s%weights_(k, 1) * real(j)
            end do
            res(j) = sum(w)
         end block
      end do
   end subroutine

end module

program gpu_metal_253
   use gpu_metal_253_mod
   implicit none

   type(stencil_t) :: s
   real, allocatable :: res(:)
   real :: column_sum, expected
   integer :: n, rows, j, k

   n = 4
   rows = 3
   allocate(s%weights_(rows, 2), res(n))
   s%weights_ = 0.0
   do k = 1, rows
      s%weights_(k, 1) = real(k)
   end do
   res = 0.0

   call apply(s, res, n)

   column_sum = 0.0
   do k = 1, rows
      column_sum = column_sum + s%weights_(k, 1)
   end do

   do j = 1, n
      expected = column_sum * real(j)
      if (abs(res(j) - expected) > 1.0e-4) error stop
   end do

   print *, res
end program
