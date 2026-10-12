module gpu_metal_425_m
  ! A function whose allocatable array result is given its shape by the
  ! reallocation of an assignment, not by an `allocate`, called inside an
  ! expression of a parallel loop. The shape is that of the operand the
  ! assigned array expression takes it from: a dummy argument, or a section
  ! of one.
  implicit none
contains
  pure function twice(c) result(r)
    real, intent(in) :: c(:)
    real, allocatable :: r(:)
    r = 2*c
  end function

  pure function twice_tail(c) result(r)
    real, intent(in) :: c(:)
    real, allocatable :: r(:)
    r = 2*c(2:)
  end function
end module

program gpu_metal_425
  use gpu_metal_425_m
  implicit none
  real :: x(3, 2), s(2), t(2)
  integer :: k
  x = reshape([1., 2., 3., 4., 5., 6.], [3, 2])
  do concurrent (k = 1:2)
    s(k) = sum(twice(x(:, k)))
    t(k) = sum(twice_tail(x(:, k)))
  end do
  print *, s, t
  if (any(s /= [12., 30.])) error stop 1
  if (any(t /= [10., 22.])) error stop 2
end program
