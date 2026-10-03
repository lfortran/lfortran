program assumed_rank_08
   implicit none
   real :: arr1d(8)
   real :: arr2d(3, 4)

   arr1d = 1.0
   arr2d = 2.0

   call check_bounds(arr1d, 1, 1, 8)
   call check_bounds(arr2d, 1, 1, 3)
   call check_bounds(arr2d, 2, 1, 4)

contains
   subroutine check_bounds(a, idim, expected_lo, expected_hi)
      real, dimension(..), intent(in) :: a
      integer, intent(in) :: idim, expected_lo, expected_hi
      integer :: lo, hi
      lo = lbound(a, dim=idim)
      hi = ubound(a, dim=idim)
      if (lo /= expected_lo) error stop "wrong lbound"
      if (hi /= expected_hi) error stop "wrong ubound"
   end subroutine check_bounds
end program assumed_rank_08
