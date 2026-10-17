module gpu_metal_403_m
  ! A function whose allocatable array result is given its shape by the
  ! reallocation of an assignment, not by an `allocate`, called inside an
  ! expression of a routine an offloaded loop calls. The shape is that of
  ! the operand the assigned array expression takes it from: a dummy
  ! argument, or a section of one.
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

  pure real function total(c) result(t)
    real, intent(in) :: c(:)
    t = sum(twice(c)) + 100*sum(twice_tail(c))
  end function
end module

program gpu_metal_403
  use gpu_metal_403_m
  implicit none
  real :: x(3, 2), s(2)
  integer :: k
  x = reshape([1., 2., 3., 4., 5., 6.], [3, 2])
  do concurrent (k = 1:2)
    s(k) = total(x(:, k))
  end do
  print *, s
  if (any(s /= [1012., 2230.])) error stop 1
end program
