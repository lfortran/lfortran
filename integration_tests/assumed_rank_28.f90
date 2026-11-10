program assumed_rank_28
   ! Regression for #16223: lbound and ubound of an assumed-rank dummy with
   ! a constant DIM. The rank of the dummy is known only when the program
   ! runs, so DIM is not checked against its declared rank (0).
   implicit none
   real :: x(3, 4)
   integer :: y(4, 3, 7)
   real, allocatable :: a(:, :)
   call check_rank2(x)
   call check_rank3(y)
   allocate(a(2:4, -1:2))
   call check_allocatable(a)
contains
   subroutine check_rank2(c)
      real, intent(in) :: c(..)
      if (ubound(c, 1) /= 3) error stop "ubound(c, 1)"
      if (ubound(c, 2) /= 4) error stop "ubound(c, 2)"
      if (lbound(c, 1) /= 1) error stop "lbound(c, 1)"
      if (lbound(c, dim=2) /= 1) error stop "lbound(c, dim=2)"
      if (ubound(c, 2, kind=8) /= 4_8) error stop "ubound(c, 2, kind=8)"
      print *, ubound(c, 1), ubound(c, 2)
      print *, lbound(c, 1), lbound(c, 2)
   end subroutine check_rank2

   subroutine check_rank3(c)
      integer, intent(in) :: c(..)
      if (lbound(c, 1) /= 1) error stop "lbound(c, 1), rank 3"
      if (lbound(c, 3) /= 1) error stop "lbound(c, 3), rank 3"
      if (ubound(c, 1) /= 4) error stop "ubound(c, 1), rank 3"
      if (ubound(c, 2) /= 3) error stop "ubound(c, 2), rank 3"
      if (ubound(c, 3) /= 7) error stop "ubound(c, 3), rank 3"
   end subroutine check_rank3

   subroutine check_allocatable(c)
      real, allocatable, intent(in) :: c(..)
      if (lbound(c, 1) /= 2) error stop "lbound(c, 1), allocatable"
      if (lbound(c, 2) /= -1) error stop "lbound(c, 2), allocatable"
      if (ubound(c, 1) /= 4) error stop "ubound(c, 1), allocatable"
      if (ubound(c, 2) /= 2) error stop "ubound(c, 2), allocatable"
   end subroutine check_allocatable
end program assumed_rank_28
