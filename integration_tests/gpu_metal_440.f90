module gpu_metal_440_m
  ! A loop that tests whether an optional argument is present runs whether
  ! it is or not, and a local allocated only when it is present is handed
  ! to the kernel unallocated otherwise.
  implicit none
contains
  subroutine s(x, y, cost, scale)
    real, intent(in) :: x(:)
    real, intent(out) :: y(:)
    real, intent(out), allocatable, optional :: cost(:)
    real, intent(in), optional :: scale
    real, allocatable :: c(:)
    integer :: i
    if (present(cost)) allocate(c(size(x)))
    do concurrent (i = 1:size(x))
      y(i) = 3*x(i)
      if (present(cost)) c(i) = 2*x(i)
      if (present(scale)) y(i) = y(i) + 1
    end do
    if (present(cost)) cost = c
  end subroutine
end module

program gpu_metal_440
  use gpu_metal_440_m
  implicit none
  real :: x(3), y(3)
  real, allocatable :: cost(:)
  x = [1., 2., 3.]
  call s(x, y)
  print *, y
  if (any(y /= [3., 6., 9.])) error stop 1
  y = 0
  call s(x, y, cost)
  print *, y, cost
  if (any(y /= [3., 6., 9.])) error stop 2
  if (any(cost /= [2., 4., 6.])) error stop 3
  call s(x, y, scale=2.)
  print *, y
  if (any(y /= [4., 7., 10.])) error stop 4
end program
