program gpu_metal_199
  ! Test: `do concurrent` loops touching double precision (real(8))
  ! ALLOCATABLE arrays must not be offloaded to Metal.  Metal Shading
  ! Language has no 64-bit floating point type, so the Metal backend lowers
  ! every real to `float`; the host, however, packs 8-byte elements into the
  ! device buffers.  The kind-8 bail-out guard used to peel only `Array`
  ! from the symbol type, so an `Allocatable(Array(Real(8)))` slipped past
  ! it and the loop was offloaded, silently producing garbage.
  implicit none

  double precision, allocatable :: d(:), x(:), y(:), m(:,:)
  integer(8), allocatable :: k(:)
  integer :: i, j

  ! whole-array section assignment inside `do concurrent`
  allocate(d(4))
  d = 0.0d0
  do concurrent (i = 1:1)
    d(:) = 1.0d0
  end do
  if (abs(sum(d) - 4.0d0) > 1.0d-12) error stop "section assign"

  ! elementwise assignment
  d = 0.0d0
  do concurrent (i = 1:4)
    d(i) = 2.5d0
  end do
  if (abs(sum(d) - 10.0d0) > 1.0d-12) error stop "elementwise assign"

  ! two allocatable double precision arrays
  allocate(x(4), y(4))
  x = 3.0d0
  y = 0.0d0
  do concurrent (i = 1:4)
    y(i) = x(i) * 2.0d0
  end do
  if (abs(sum(y) - 24.0d0) > 1.0d-12) error stop "two arrays"

  ! two dimensional allocatable double precision array
  allocate(m(2,2))
  m = 0.0d0
  do concurrent (i = 1:2)
    do concurrent (j = 1:2)
      m(i,j) = 1.5d0
    end do
  end do
  if (abs(sum(m) - 6.0d0) > 1.0d-12) error stop "2-D array"

  ! double precision mixed with an integer(8) allocatable
  allocate(k(4))
  d = 0.0d0
  k = 0_8
  do concurrent (i = 1:4)
    d(i) = 1.0d0
    k(i) = 3_8
  end do
  if (abs(sum(d) - 4.0d0) > 1.0d-12) error stop "mixed real(8)"
  if (sum(k) /= 12_8) error stop "mixed integer(8)"

  print *, "PASS"
end program gpu_metal_199
