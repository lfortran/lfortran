program assumed_rank_29
   ! Regression for #16223: lbound and ubound without DIM of an assumed-rank
   ! dummy. The size of the result is the rank of the actual argument, which
   ! is known only when the program runs.
   implicit none
   real :: x(3, 4)
   real :: y(4, 3, 7)
   real :: z
   real, allocatable :: a(:, :)
   call check(x, [1, 1], [3, 4])
   call check(y, [1, 1, 1], [4, 3, 7])
   call check(z, [integer ::], [integer ::])
   allocate(a(2:4, -1:2))
   call check_allocatable(a, [2, -1], [4, 2])
contains
   subroutine check(c, lb, ub)
      real, intent(in) :: c(..)
      integer, intent(in) :: lb(:), ub(:)
      integer :: bounds(rank(c))
      integer(8) :: bounds8(rank(c))
      if (size(lbound(c)) /= size(lb)) error stop "size(lbound(c))"
      if (size(ubound(c)) /= size(ub)) error stop "size(ubound(c))"
      if (any(lbound(c) /= lb)) error stop "lbound(c)"
      if (any(ubound(c) /= ub)) error stop "ubound(c)"
      if (any(ubound(c) /= shape(c))) error stop "ubound(c) /= shape(c)"
      bounds = lbound(c)
      if (any(bounds /= lb)) error stop "bounds = lbound(c)"
      bounds = ubound(c)
      if (any(bounds /= ub)) error stop "bounds = ubound(c)"
      bounds8 = ubound(c, kind=8)
      if (any(bounds8 /= ub)) error stop "bounds8 = ubound(c, kind=8)"
      print *, lbound(c)
      print *, ubound(c)
   end subroutine check

   subroutine check_allocatable(c, lb, ub)
      real, allocatable, intent(in) :: c(..)
      integer, intent(in) :: lb(:), ub(:)
      if (size(lbound(c)) /= size(lb)) error stop "size(lbound(c)), allocatable"
      if (any(lbound(c) /= lb)) error stop "lbound(c), allocatable"
      if (any(ubound(c) /= ub)) error stop "ubound(c), allocatable"
      print *, lbound(c)
      print *, ubound(c)
   end subroutine check_allocatable
end program assumed_rank_29
