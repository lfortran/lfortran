program transfer_28
  implicit none

  type :: pair
    integer :: a
    integer :: b
  end type

  type(pair) :: vals(2)
  integer, allocatable :: ints(:)

  vals(1) = pair(1, 2)
  vals(2) = pair(3, 4)
  ints = transfer(vals, ints)

  if (size(ints) /= 4) error stop
  if (ints(1) /= 1) error stop
  if (ints(2) /= 2) error stop
  if (ints(3) /= 3) error stop
  if (ints(4) /= 4) error stop
  print *, "PASS"
end program
