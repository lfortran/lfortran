program assumed_rank_27
   ! Regression for #16361: an automatic array sized by rank() of an
   ! assumed-rank dummy, assigned from shape() of that dummy. The result of
   ! shape() is allocatable, so it needs a temporary when it is returned
   ! into the non-allocatable automatic array.
   implicit none
   call check_kind8(7)
   call check_kind8([8, 9])
   call check_kind8(reshape([1, 2, 3, 4, 5, 6], [2, 3]))
   call check_default_kind(7)
   call check_default_kind([8, 9])
   call check_default_kind(reshape([1, 2, 3, 4, 5, 6], [2, 3]))
   call check_rank_loop(reshape([1, 2, 3, 4, 5, 6], [2, 3]))
contains
   subroutine check_kind8(value)
      integer, intent(in) :: value(..)
      integer(8) :: dims(rank(value))
      integer :: i
      dims = shape(value, 8)
      if (size(dims) /= rank(value)) error stop "kind 8: wrong size"
      do i = 1, size(dims)
         if (dims(i) /= size(value, i)) error stop "kind 8: wrong extent"
      end do
   end subroutine check_kind8

   subroutine check_default_kind(value)
      integer, intent(in) :: value(..)
      integer :: dims(rank(value))
      integer :: i
      dims = shape(value)
      if (size(dims) /= rank(value)) error stop "default kind: wrong size"
      do i = 1, size(dims)
         if (dims(i) /= size(value, i)) error stop "default kind: wrong extent"
      end do
   end subroutine check_default_kind

   subroutine check_rank_loop(value)
      integer, intent(in) :: value(..)
      integer :: i, n
      n = 0
      do i = 1, rank(value)
         n = n + 1
      end do
      if (n /= rank(value)) error stop "rank loop bound"
   end subroutine check_rank_loop
end program assumed_rank_27
