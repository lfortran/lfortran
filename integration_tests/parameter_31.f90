module parameter_31_m
  implicit none
  integer, parameter :: ia(0:2) = [1, 2, 3]
end module

program parameter_31
  ! Sections of named-constant arrays whose lower bound is not 1
  use parameter_31_m, only: ia
  implicit none
  real(8), parameter :: w(0:3) = [10.0d0, 20.0d0, 30.0d0, 40.0d0]
  integer, parameter :: m(-1:1, 2:3) = reshape([1, 2, 3, 4, 5, 6], [3, 2])
  integer :: v(3), b(2)

  v = ia(:)
  if (any(v /= [1, 2, 3])) error stop 1
  v = ia(0:2)
  if (any(v /= [1, 2, 3])) error stop 2
  v = 10 * ia(:)
  if (any(v /= [10, 20, 30])) error stop 3
  b = ia(1:2)
  if (any(b /= [2, 3])) error stop 4
  b = ia(2:0:-2)
  if (any(b /= [3, 1])) error stop 5
  if (sum(ia(:)) /= 6) error stop 6

  if (any(abs(w(1:3) - [20.0d0, 30.0d0, 40.0d0]) > 1.0d-12)) error stop 7
  if (abs(sum(w(:)) - 100.0d0) > 1.0d-12) error stop 8

  v = m(:, 3)
  if (any(v /= [4, 5, 6])) error stop 9
  b = m(0, :)
  if (any(b /= [2, 5])) error stop 10
  b = m(1, 2:3)
  if (any(b /= [3, 6])) error stop 11

  print '(3i3)', ia(:)
end program
