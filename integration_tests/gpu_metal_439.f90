module gpu_metal_439_m
  ! A routine the loop calls with an optional array dummy argument and an
  ! allocatable local sized by another dummy: the array a call is handed for
  ! the routine's temporary of `b + w` is sized on the host from the extent
  ! of the section passed for `x`, which the call binds to a name, also when
  ! `w` is absent.
  implicit none
contains
  pure real function al(x, w)
    real, intent(in) :: x(:)
    real, intent(in), optional :: w(:)
    real, allocatable :: b(:)
    allocate(b(size(x)))
    b = x
    if (present(w)) b = b + w
    al = sum(b)
  end function
end module

program gpu_metal_439
  use gpu_metal_439_m
  implicit none
  real :: y(3), a(2,3)
  integer :: i
  a = 1
  do concurrent (i = 1:3)
    y(i) = al(a(:, i))
  end do
  if (any(y /= 2)) error stop 1
  do concurrent (i = 1:3)
    y(i) = al(a(:, i), a(:, i))
  end do
  if (any(y /= 4)) error stop 2
  print *, "ok"
end program
