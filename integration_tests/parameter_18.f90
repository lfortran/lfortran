program parameter_18
  implicit none

  integer, parameter :: grid(2, 2) = reshape([1, 2, 2, 1], [2, 2])
  integer, parameter :: where_is_two(2) = minloc(grid, dim=1, mask=grid > 1, back=.true.)

  print *, "minloc:", where_is_two

  if (any(where_is_two /= [2, 1])) error stop
end program