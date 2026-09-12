! Test: a decomposed allocatable component whose name is also the name of
! another kernel argument, compiled with --gpu=metal.
!
! The kernel layout describes the component by its symbol, so the component
! `v` of `t` and the plain array `v` are two different arguments even though
! they are spelled the same.
program gpu_metal_336
  implicit none
  type :: t
    real, allocatable :: v(:)
  end type
  type(t) :: arr(2)
  real :: v(2)
  integer :: i

  v = [3.0, 4.0]
  do i = 1, 2
    allocate(arr(i)%v(2))
    arr(i)%v = 0.0
  end do

  do concurrent (i = 1:2)
    arr(i)%v(1) = v(i)
    arr(i)%v(2) = v(i) + 1.0
  end do

  if (abs(arr(1)%v(1) - 3.0) > 1e-5) error stop
  if (abs(arr(1)%v(2) - 4.0) > 1e-5) error stop
  if (abs(arr(2)%v(1) - 4.0) > 1e-5) error stop
  if (abs(arr(2)%v(2) - 5.0) > 1e-5) error stop
  print *, "PASS"
end program
