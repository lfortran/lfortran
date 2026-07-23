program assumed_rank_13
  implicit none

  integer, allocatable :: x(:, :)
  integer, allocatable :: y(:)

  allocate(x(2, 3))
  y = make_array(x)
  if (size(y) /= 2) error stop

contains

  function make_array(arg) result(res)
    integer, allocatable :: arg(..)
    integer, allocatable :: res(:)

    allocate(res(rank(arg)))
    res = 1
  end function make_array

end program assumed_rank_13
