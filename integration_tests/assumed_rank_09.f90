program assumed_rank_09
   ! Regression: `shape(x)` of an assumed-rank dummy argument must work both
   ! when its result is assigned to an allocatable rank-1 array (the helper
   ! function must self-allocate its result) and when the result is consumed
   ! by another intrinsic that pre-allocates a temporary buffer
   ! (the helper must NOT re-allocate in that case).
   !
   ! Also regression for `rank(x)` and `shape(x)` evaluated inside the
   ! `rank default` branch of a `select rank`: the actual runtime rank of
   ! the selector must be used, not the rank of the preceding `rank(N)`
   ! case (whose entry would otherwise leak into the default body).
   implicit none
   real    :: r1(3)        = [1.0, 2.0, 3.0]
   real    :: r2(2, 4)     = 0.0
   real    :: r3(2, 3, 4)  = 0.0
   real    :: r4(2, 2, 2, 2) = 0.0
   real    :: r5(2, 2, 2, 2, 2) = 0.0
   integer :: int_arr(5)   = 0
   integer :: matrix(3, 7) = 0

   call check_unlimited(r1,  [3])
   call check_unlimited(r2,  [2, 4])
   call check_unlimited(r3,  [2, 3, 4])
   call check_unlimited(int_arr, [5])
   call check_unlimited(matrix,  [3, 7])

   call check_type_star(r1, 3)
   call check_type_star(r2, 8)
   call check_type_star(r3, 24)

   call check_rank_default(r3, 3, [2, 3, 4])
   call check_rank_default(r4, 4, [2, 2, 2, 2])
   call check_rank_default(r5, 5, [2, 2, 2, 2, 2])
contains
   subroutine check_unlimited(input, expected_shape)
      class(*),  dimension(..), intent(in) :: input
      integer,                  intent(in) :: expected_shape(:)
      integer, allocatable :: s(:)
      integer :: i
      s = shape(input)
      if (size(s) /= size(expected_shape)) error stop "rank mismatch"
      do i = 1, size(s)
         if (s(i) /= expected_shape(i)) error stop "shape mismatch"
      end do
   end subroutine check_unlimited

   subroutine check_type_star(a, expected_size)
      type(*), dimension(..), intent(in), contiguous :: a
      integer,                intent(in)             :: expected_size
      integer :: n
      n = product(shape(a))
      if (n /= expected_size) error stop "product(shape()) mismatch"
   end subroutine check_type_star

   subroutine check_rank_default(input, expected_rank, expected_shape)
      class(*), dimension(..), intent(in) :: input
      integer,                 intent(in) :: expected_rank
      integer,                 intent(in) :: expected_shape(:)
      integer, allocatable :: s(:)
      integer :: i
      select rank(input)
      rank(0); error stop "rank(0) reached unexpectedly"
      rank(1); error stop "rank(1) reached unexpectedly"
      rank(2); error stop "rank(2) reached unexpectedly"
      rank default
         if (rank(input) /= expected_rank) error stop "rank() in rank default"
         s = shape(input)
         if (size(s) /= expected_rank) error stop "shape() size in rank default"
         do i = 1, size(s)
            if (s(i) /= expected_shape(i)) error stop "shape() value in rank default"
         end do
      end select
   end subroutine check_rank_default
end program assumed_rank_09
