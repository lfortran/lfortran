module gpu_metal_201_m
  implicit none
  type :: box_t
    real, allocatable :: v(:)
  end type
contains
  ! The component is given a shape by an allocate rather than by the
  ! assignment, and is filled from a section rather than from a whole
  ! argument, so the host has to read the shape from the allocate to give the
  ! component storage before the kernel runs.
  pure function head(x, n) result(r)
    real, intent(in) :: x(:)
    integer, intent(in) :: n
    type(box_t) :: r
    allocate(r%v(n))
    r%v = x(1:n)
  end function
end module

program gpu_metal_201
  use gpu_metal_201_m
  implicit none
  type(box_t) :: b(2)
  real :: s(4)
  integer :: i
  s = [1.0, 2.0, 3.0, 4.0]
  do concurrent (i = 1:2)
    b(i) = head(s, 3)
  end do
  if (size(b(1)%v) /= 3) error stop
  if (abs(b(1)%v(3) - 3.0) > 1e-5) error stop
  if (abs(b(2)%v(1) - 1.0) > 1e-5) error stop
  print *, "ok"
end program
