program gpu_metal_482
  ! A `do concurrent` loop with an empty index range launches no thread:
  ! the arrays it would write keep their values, also when an earlier loop
  ! left them on the device, and a reduction over it keeps its initial value.
  implicit none
  integer :: i, n, lo
  integer :: a(5)
  integer, allocatable :: untouched(:)
  real :: x(100), s
  n = 3
  lo = 7

  a = -1
  do concurrent (i = lo:n)
    a(i) = 0
  end do
  if (any(a /= -1)) error stop 1

  ! No descriptor access or allocation is needed for an unused argument.
  do concurrent (i = lo:n)
    untouched(i) = 0
  end do
  if (allocated(untouched)) error stop 5

  do concurrent (i = 1:5)
    a(i) = i
  end do
  do concurrent (i = lo:n)
    a(i) = 0
  end do
  do i = 1, 5
    if (a(i) /= i) error stop 2
  end do

  x = 1
  s = 5
  do concurrent (i = lo:n) reduce(+:s)
    s = s + x(i)
  end do
  if (abs(s - 5) > 1e-6) error stop 3

  s = 0
  do concurrent (i = 1:100) reduce(+:s)
    s = s + x(i)
  end do
  if (abs(s - 100) > 1e-6) error stop 4
  print *, a, s
end program
