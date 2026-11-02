module gpu_metal_606_m
  ! The host sizes the automatic arrays of these routines from the section
  ! the loop passes for `v`, whose integer(8) bound makes size(v) an
  ! integer(8) expression there. It meets default integers in a max(), in a
  ! comparison and as the argument of merge().
  implicit none
contains
  pure real function gmax(v) result(r)
    real, intent(in) :: v(:)
    real :: w(max(1, size(v)))
    w = 2
    r = sum(w)
  end function

  pure real function gmerge(v) result(r)
    real, intent(in) :: v(:)
    real :: w(merge(size(v), 1, size(v) > 0))
    w = 2
    r = sum(w)
  end function
end module

program gpu_metal_606
  use gpu_metal_606_m
  implicit none
  real :: x(3, 2), s(2)
  integer :: k
  integer(8) :: k8
  x = 1
  k8 = 2
  do concurrent (k = 1:2)
    s(k) = k*gmax(x(k8:, k))
  end do
  if (any(s /= [4., 8.])) error stop 1
  do concurrent (k = 1:2)
    s(k) = k*gmerge(x(k8:, k))
  end do
  if (any(s /= [4., 8.])) error stop 2
  k8 = 4
  do concurrent (k = 1:2)
    s(k) = k*gmax(x(k8:, k))
  end do
  if (any(s /= [2., 4.])) error stop 3
  do concurrent (k = 1:2)
    s(k) = k*gmerge(x(k8:, k))
  end do
  if (any(s /= [2., 4.])) error stop 4
  print *, "ok"
end program
