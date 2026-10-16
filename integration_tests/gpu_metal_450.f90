module gpu_metal_450_m
  ! The contiguous copy a launch makes of an assumed-shape argument is
  ! released after the launch, so the same loop can be launched again.
  ! A type-bound procedure keeps the arguments assumed-shape.
  implicit none
  type :: t
  contains
    procedure :: run
  end type
contains
  subroutine run(self, out, inp)
    class(t), intent(in) :: self
    real, intent(inout) :: out(:)
    real, intent(in) :: inp(:)
    integer :: i, k
    do k = 1, 3
      do concurrent (i = 1:size(out))
        out(i) = out(i) + inp(i)
      end do
    end do
  end subroutine
end module

program gpu_metal_450
  use gpu_metal_450_m
  implicit none
  type(t) :: o
  real :: x(8), y(8)
  integer :: i
  x = 0
  y = [(real(i), i = 1, 8)]
  call o%run(x(1:8:2), y(2:8:2))
  call o%run(x(1:8:2), y(2:8:2))
  print *, x
  do i = 1, 8, 2
    if (x(i) /= 6 * y(i + 1)) error stop 1
  end do
  if (any(x(2:8:2) /= 0)) error stop 2
end program
