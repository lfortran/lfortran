! Test: merge() with allocatable array-section mask inside struct constructor
! in do concurrent, compiled with --gpu=metal.
! Exercises VLA workspace dimension resolution through ArraySize + Associate
! when the mask array is allocatable (runtime descriptor dimensions).
program gpu_metal_171
  implicit none
  type :: t
    real, allocatable :: v(:)
  end type
  type(t) :: arr(2)
  logical, allocatable :: m(:,:)
  integer :: i

  allocate(m(2, 2))
  m = .true.

  do i = 1, 2
    allocate(arr(i)%v(2))
    arr(i)%v = 0.0
  end do

  do concurrent(i=1:2)
    arr(i) = t(merge(1.0, 0.0, m(:,i)))
  end do

  if (abs(arr(1)%v(1) - 1.0) > 1e-5) error stop
  if (abs(arr(1)%v(2) - 1.0) > 1e-5) error stop
  if (abs(arr(2)%v(1) - 1.0) > 1e-5) error stop
  if (abs(arr(2)%v(2) - 1.0) > 1e-5) error stop
  print *, "PASS"
end program
