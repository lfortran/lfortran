module gpu_metal_181_m
  implicit none
  type :: t
    real, allocatable :: v(:)
  end type
end module

program gpu_metal_181
  ! Test: function returning DT with allocatable component called inside
  ! do concurrent. The Metal codegen must emit the correct number of
  ! arguments (including data pointer and size for the allocatable
  ! member) and the host must pre-allocate workspace for unallocated
  ! struct members before kernel launch.
  use gpu_metal_181_m
  implicit none
  type(t) :: a(2)
  real :: x(1)
  integer :: i
  x(1) = 1.0
  do concurrent (i = 1:2)
    a(i) = construct(x)
  end do
  if (abs(a(1)%v(1) - 1.0) > 1e-5) error stop
  if (abs(a(2)%v(1) - 1.0) > 1e-5) error stop
  print *, "ok"
contains
  pure function construct(x) result(r)
    real, intent(in) :: x(:)
    type(t) :: r
    r%v = x
  end function
end program
