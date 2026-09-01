program gpu_metal_214
  ! Test: double precision values that reach a Metal kernel through the
  ! by-value `__ScalarArgs` struct, or as a kernel-local temporary, must
  ! also keep the `do concurrent` on the CPU.  An `Allocatable(Real(8))`
  ! scalar used to be emitted as a `float` struct member while the host
  ! packed 8 bytes for it — a size mismatch independent of the device
  ! buffers.
  implicit none

  double precision, allocatable :: a
  double precision :: b
  real, allocatable :: r(:)
  double precision, allocatable :: q(:)
  double precision :: t
  integer :: i

  ! allocatable double precision SCALAR read inside the loop
  allocate(a)
  a = 3.0d0
  allocate(q(4))
  q = 0.0d0
  do concurrent (i = 1:4)
    q(i) = a
  end do
  if (abs(sum(q) - 12.0d0) > 1.0d-12) error stop "allocatable dp scalar"

  ! plain double precision scalar passed by value
  b = 5.0d0
  q = 0.0d0
  do concurrent (i = 1:4)
    q(i) = b
  end do
  if (abs(sum(q) - 20.0d0) > 1.0d-12) error stop "dp scalar arg"

  ! double precision kernel-local temporary feeding a real(4) array
  allocate(r(4))
  r = 0.0
  do concurrent (i = 1:4)
    t = 2.0d0 * dble(i)
    r(i) = real(t)
  end do
  if (abs(sum(r) - 20.0) > 1.0e-5) error stop "dp local temporary"

  print *, "PASS"
end program gpu_metal_214
