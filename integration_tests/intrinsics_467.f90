program intrinsics_468
  implicit none

  integer, allocatable :: a(:, :)
  integer, allocatable :: b(:)

  allocate(a(2, 3))
  a = reshape([1, 4, 2, 6, 3, 5], [2, 3])
  ! a is:
  ! 1  2  3
  ! 4  6  5

  ! maxloc
  a(1, :) = maxloc(a, dim=1)
  if (any(a(1, :) /= [2, 2, 2])) error stop 1
  a = reshape([1, 4, 2, 6, 3, 5], [2, 3])
  a(:, 1) = maxloc(a, dim=2)
  if (any(a(:, 1) /= [3, 2])) error stop 2
  a = reshape([1, 4, 2, 6, 3, 5], [2, 3])
  allocate(b(2))
  b = maxloc(a)
  if (any(b /= [2, 2])) error stop 3

  a = reshape([1, 4, 2, 6, 3, 5], [2, 3])

  ! minloc
  a(1, :) = minloc(a, dim=1)
  if (any(a(1, :) /= [1, 1, 1])) error stop 4
  a = reshape([1, 4, 2, 6, 3, 5], [2, 3])
  a(:, 1) = minloc(a, dim=2)
  if (any(a(:, 1) /= [1, 1])) error stop 5
  a = reshape([1, 4, 2, 6, 3, 5], [2, 3])
  b = minloc(a)
  if (any(b /= [1, 1])) error stop 6

  a = reshape([1, 4, 2, 6, 3, 5], [2, 3])

  ! findloc
  a(1, :) = findloc(a, 6, dim=1)
  if (any(a(1, :) /= [0, 2, 0])) error stop 7
  a = reshape([1, 4, 2, 6, 3, 5], [2, 3])
  a(:, 1) = findloc(a, 6, dim=2)
  if (any(a(:, 1) /= [0, 2])) error stop 8
  a = reshape([1, 4, 2, 6, 3, 5], [2, 3])
  b = findloc(a, 6)
  if (any(b /= [2, 2])) error stop 9

  a = reshape([1, 4, 2, 6, 3, 5], [2, 3])

  ! product
  a(1, :) = product(a, dim=1)
  if (any(a(1, :) /= [4, 12, 15])) error stop 10
  a = reshape([1, 4, 2, 6, 3, 5], [2, 3])
  a(:, 1) = product(a, dim=2)
  if (any(a(:, 1) /= [6, 120])) error stop 11

  a = reshape([1, 4, 2, 6, 3, 5], [2, 3])

  ! iparity
  a(1, :) = iparity(a, dim=1)
  if (any(a(1, :) /= [5, 4, 6])) error stop 12

end program
