program gpu_metal_159
  implicit none
  type :: t
    integer :: m
    real, allocatable :: b(:)
  end type
  type(t) :: x
  real :: r(4)
  x%m = 3
  allocate(x%b(3))
  x%b = 0.5
  r = 0.0
  call sub(x, r)
  if (abs(r(1) - 1.5) > 1e-6) error stop
  if (abs(r(2) - 1.5) > 1e-6) error stop
  if (abs(r(3) - 1.5) > 1e-6) error stop
  if (abs(r(4) - 1.5) > 1e-6) error stop
  print *, "ok"
contains
  subroutine sub(x, r)
    type(t), intent(in) :: x
    real, intent(inout) :: r(:)
    integer :: i, n(x%m)
    n = 1
    do concurrent (i = 1:size(r))
      r(i) = n(1) + x%b(1)
    end do
  end subroutine
end program
