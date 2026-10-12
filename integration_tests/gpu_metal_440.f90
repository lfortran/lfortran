module gpu_metal_440_m
  ! The temporary of the array expression `2*v` a routine the loop calls
  ! passes to a function has the extent of `v`, which the host reads from
  ! the section the loop passes for it (#14652).
  implicit none
contains
  pure function total(v) result(r)
    real, intent(in) :: v(:)
    real :: r
    r = sum(v)
  end function

  pure function h(v) result(r)
    real, intent(in) :: v(:)
    real :: r
    r = 1 + total(2*v)
  end function
end module

program gpu_metal_440
  use gpu_metal_440_m
  implicit none
  real :: x(3, 2), s(2)
  integer :: k
  x = reshape([1., 2., 3., 4., 5., 6.], [3, 2])
  do concurrent (k = 1:2)
    s(k) = h(x(:, k))
  end do
  if (any(s /= [13., 31.])) error stop 1
  do concurrent (k = 1:2)
    s(k) = 2*h(x(:, k))
  end do
  if (any(s /= [26., 62.])) error stop 2
  print *, "ok"
end program
