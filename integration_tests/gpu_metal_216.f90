program gpu_metal_216
  ! Test: a double precision POINTER array in a `do concurrent`.  The type
  ! is `Pointer(Array(Real(8)))`; the kind-8 bail-out guard did not peel
  ! `Pointer` either, so the loop was offloaded to Metal and the compiler
  ! then hit an internal assertion.  It must simply stay on the CPU.
  implicit none

  double precision, target, allocatable :: t(:)
  double precision, pointer :: p(:)
  integer :: i

  allocate(t(4))
  t = 0.0d0
  p => t

  do concurrent (i = 1:4)
    p(i) = 1.25d0
  end do
  if (abs(sum(t) - 5.0d0) > 1.0d-12) error stop "dp pointer elementwise"

  do concurrent (i = 1:1)
    p(:) = 2.0d0
  end do
  if (abs(sum(t) - 8.0d0) > 1.0d-12) error stop "dp pointer section"

  nullify(p)
  print *, "PASS"
end program gpu_metal_216
