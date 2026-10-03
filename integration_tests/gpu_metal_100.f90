! Test: sum() on allocatable member of array-of-struct inside do concurrent.
! Previously generated invalid Metal shader due to missing ArrayBound/ArraySize
! handling for subscripted derived type allocatable members.
program gpu_metal_100
  implicit none
  type :: tensor_t
    real, allocatable :: v(:)
  end type
  integer :: i
  type(tensor_t) :: t(3)
  real :: r(3)
  allocate(t(1)%v(2), t(2)%v(3), t(3)%v(1))
  t(1)%v = [1.0, 2.0]
  t(2)%v = [3.0, 4.0, 5.0]
  t(3)%v = [6.0]
  do concurrent (i = 1:3)
    r(i) = sum(t(i)%v)
  end do
  if (abs(r(1) - 3.0) > 1e-5) error stop
  if (abs(r(2) - 12.0) > 1e-5) error stop
  if (abs(r(3) - 6.0) > 1e-5) error stop
  print *, "PASS"
end program
