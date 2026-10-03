program select_rank_35
    real, allocatable :: padded1(:)
   real, allocatable :: padded2(:,:)
   real :: c1 = 7.0
   real :: c2 = -3.5

   call pad1(padded1, c1)
   if (size(padded1) /= 4) error stop "rank-1: wrong size"
   if (any(abs(padded1 - 7.0) > 1.0e-6)) error stop "rank-1: wrong values"

   call pad2(padded2, c2)
   if (size(padded2,1) /= 2) error stop "rank-2: wrong dim 1"
   if (size(padded2,2) /= 3) error stop "rank-2: wrong dim 2"
   if (any(abs(padded2 + 3.5) > 1.0e-6)) error stop "rank-2: wrong values"

contains

   subroutine pad1(data_padded, val)
      real, allocatable, dimension(..), intent(out) :: data_padded
      real, intent(in) :: val
      select rank(data_padded)
      rank(1)
         allocate(data_padded(1:4), source = val)
      end select
   end subroutine pad1

   subroutine pad2(data_padded, val)
      real, allocatable, dimension(..), intent(out) :: data_padded
      real, intent(in) :: val
      select rank(data_padded)
      rank(2)
         allocate(data_padded(1:2,1:3), source = val)
      end select
   end subroutine pad2
end program select_rank_35