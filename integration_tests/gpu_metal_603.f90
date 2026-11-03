module gpu_metal_603_m
  ! The host sizes the temporary of `2*v` from the section the loop passes
  ! for `v`. A section with an integer(8) bound has an integer(8) extent,
  ! to which the extent's derivation adds a default integer 1.
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

program gpu_metal_603
  use gpu_metal_603_m
  implicit none
  real :: x(3, 2), s(2)
  integer :: k
  integer(8) :: k8
  x = reshape([1., 2., 3., 4., 5., 6.], [3, 2])
  k8 = 1
  do concurrent (k = 1:2)
    s(k) = h(x(k8:, k))
  end do
  if (any(s /= [13., 31.])) error stop 1
  do concurrent (k = 1:2)
    s(k) = 2*h(x(k8:, k))
  end do
  if (any(s /= [26., 62.])) error stop 2
  k8 = 2
  do concurrent (k = 1:2)
    s(k) = h(x(:k8, k))
  end do
  if (any(s /= [7., 19.])) error stop 3
  print *, "ok"
end program
