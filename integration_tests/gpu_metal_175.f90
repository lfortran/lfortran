module gpu_metal_175_m
  implicit none
  type :: t
    real, allocatable :: v(:)
  end type
contains
  pure function get_v(x) result(r)
    type(t), intent(in) :: x
    real, allocatable :: r(:)
    allocate(r(size(x%v)))
    r = x%v
  end function
end module

program gpu_metal_175
  use gpu_metal_175_m
  implicit none
  type(t) :: arr(2)
  real :: a(2, 2), b(2, 2)
  real :: expected(2, 2)
  integer :: i

  arr(1) = t([1.0, 2.0])
  arr(2) = t([3.0, 4.0])
  expected = reshape([1.0, 2.0, 3.0, 4.0], [2, 2])

  do concurrent(i = 1:2)
    a(:, i) = arr(i)%v
  end do

  do concurrent(i = 1:2)
    b(:, i) = get_v(arr(i))
  end do

  if (any(abs(a - expected) > 1e-6)) error stop
  if (any(abs(b - expected) > 1e-6)) error stop
  print *, "PASS"
end program
