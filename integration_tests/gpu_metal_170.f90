! Test: merge() with array-section comparison inside struct constructor
! in do concurrent, compiled with --gpu=metal.
! Exercises VLA workspace dimension resolution through Allocate statements
! and correct Metal codegen for struct member writes via decomposed buffers.
program gpu_metal_170
  implicit none
  type :: t
    real, allocatable :: v(:)
  end type
  type(t) :: arr(2)
  real :: x(2, 2)
  integer :: i

  x = 0.3

  ! Pre-allocate struct members so Metal decomposed buffers are set up
  do i = 1, 2
    allocate(arr(i)%v(2))
    arr(i)%v = 0.0
  end do

  do concurrent(i=1:2)
    arr(i) = t(merge(1.0, 0.0, x(:,i) < 0.5))
  end do

  ! All x values are 0.3 < 0.5, so merge returns 1.0 for all
  if (abs(arr(1)%v(1) - 1.0) > 1e-5) error stop
  if (abs(arr(1)%v(2) - 1.0) > 1e-5) error stop
  if (abs(arr(2)%v(1) - 1.0) > 1e-5) error stop
  if (abs(arr(2)%v(2) - 1.0) > 1e-5) error stop
  print *, "PASS"
end program
