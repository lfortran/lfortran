! Test: do concurrent accessing allocatable member of an allocatable
! array of derived types with --gpu=metal.
! Previously produced garbage values because the host-side LLVM codegen
! did not create the auxiliary data/offsets/sizes Metal buffers for
! allocatable (dynamically-sized) arrays of structs.
program gpu_metal_97
  implicit none
  type :: tensor_t
    real, allocatable :: v(:)
  end type
  type(tensor_t), allocatable :: a(:)
  real :: r(3)
  integer :: i
  allocate(a(3))
  allocate(a(1)%v(1))
  allocate(a(2)%v(1))
  allocate(a(3)%v(1))
  a(1)%v(1) = 10.0
  a(2)%v(1) = 20.0
  a(3)%v(1) = 30.0
  do concurrent (i = 1:3)
    r(i) = a(i)%v(1)
  end do
  if (abs(r(1) - 10.0) > 1e-5) error stop
  if (abs(r(2) - 20.0) > 1e-5) error stop
  if (abs(r(3) - 30.0) > 1e-5) error stop
  print *, "PASS"
end program
