program arrays_123
  implicit none
  type :: t
    integer :: a
  end type
  type(t), parameter :: v(2,1) = reshape([t(1), t(2)], [2, 1])
  type(t) :: r(2)
  r = v(:, 1)
  if (r(1)%a /= 1) error stop
  if (r(2)%a /= 2) error stop
  print *, r(1)%a, r(2)%a
end program
