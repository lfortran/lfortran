! Test shape() on allocatable assumed-rank arrays inside nested select rank.
! Regression: is_assumed_rank_array did not look through Allocatable wrappers,
! so shape(a) returned a 0-element array when a was allocatable assumed-rank.
program select_rank_18
  implicit none
  real, allocatable :: x(:,:), y(:,:)
  integer :: s(2)
  allocate(x(3,4), y(3,4))
  x = 1.0
  y = 1.0
  call test_sub(x, y)
contains
  subroutine test_sub(a, b)
    real, allocatable, intent(inout) :: a(..)
    real, intent(in) :: b(..)
    integer :: sa(2), sb(2)
    select rank(a)
      rank(2)
        select rank(b)
          rank(2)
            if (any(shape(a) /= shape(b))) error stop
            sa = shape(a)
            sb = shape(b)
            if (sa(1) /= 3) error stop
            if (sa(2) /= 4) error stop
            if (sb(1) /= 3) error stop
            if (sb(2) /= 4) error stop
        end select
    end select
  end subroutine
end program
