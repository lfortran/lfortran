! Test: nested implied-do array constructor where the inner expression
! references the outer loop variable but the inner loop bounds do not.
! [([(i+j, j=1,M)], i=1,N)] must produce M*N elements, not N*(N+1)/2.
program implied_do_loops24
  implicit none
  integer :: i, j
  integer, allocatable :: a(:)

  ! Basic case: inner expr references outer var, constant bounds
  a = [([(i + j, j = 1, 3)], i = 1, 3)]
  if (size(a) /= 9) error stop "test 1: wrong size"
  ! Expected: [2,3,4, 3,4,5, 4,5,6]
  if (a(1) /= 2) error stop "test 1: a(1)"
  if (a(2) /= 3) error stop "test 1: a(2)"
  if (a(3) /= 4) error stop "test 1: a(3)"
  if (a(4) /= 3) error stop "test 1: a(4)"
  if (a(5) /= 4) error stop "test 1: a(5)"
  if (a(6) /= 5) error stop "test 1: a(6)"
  if (a(7) /= 4) error stop "test 1: a(7)"
  if (a(8) /= 5) error stop "test 1: a(8)"
  if (a(9) /= 6) error stop "test 1: a(9)"

  ! Non-square case: M > N
  a = [([(i * j, j = 1, 4)], i = 1, 2)]
  if (size(a) /= 8) error stop "test 2: wrong size"
  ! Expected: [1,2,3,4, 2,4,6,8]
  if (a(1) /= 1) error stop "test 2: a(1)"
  if (a(4) /= 4) error stop "test 2: a(4)"
  if (a(5) /= 2) error stop "test 2: a(5)"
  if (a(8) /= 8) error stop "test 2: a(8)"

  ! Non-square case: M < N (under-allocation was the original crash)
  a = [([(i + j, j = 1, 2)], i = 1, 4)]
  if (size(a) /= 8) error stop "test 3: wrong size"
  ! Expected: [2,3, 3,4, 4,5, 5,6]
  if (a(1) /= 2) error stop "test 3: a(1)"
  if (a(2) /= 3) error stop "test 3: a(2)"
  if (a(7) /= 5) error stop "test 3: a(7)"
  if (a(8) /= 6) error stop "test 3: a(8)"

  print *, "All tests passed."
end program
