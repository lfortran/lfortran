program associate_42
  implicit none
  integer :: c

  ! Test: array constructor with implied-do inside associate
  ! The inner [(0d0, c=1,3)] expands to 3 elements, so total size is 5
  associate(b => [-1d0, [(0d0, c = 1, 3)], 1d0])
    if (size(b) /= 5) error stop
    if (b(1) /= -1d0) error stop
    if (b(2) /= 0d0) error stop
    if (b(3) /= 0d0) error stop
    if (b(4) /= 0d0) error stop
    if (b(5) /= 1d0) error stop
    print *, "ok, size =", size(b)
  end associate

  ! Test with explicit step and different values
  associate(a => [10, [(i_square(c), c = 1, 4)], 99])
    if (size(a) /= 6) error stop
    if (a(1) /= 10) error stop
    if (a(2) /= 1) error stop
    if (a(3) /= 4) error stop
    if (a(4) /= 9) error stop
    if (a(5) /= 16) error stop
    if (a(6) /= 99) error stop
  end associate

contains

  pure integer function i_square(n)
    integer, intent(in) :: n
    i_square = n * n
  end function

end program
