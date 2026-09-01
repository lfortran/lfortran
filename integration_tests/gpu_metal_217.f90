program gpu_metal_217
  ! Test: types Metal Shading Language cannot represent at the host's width.
  ! MSL has no complex type at all (the backend used to emit an invalid
  ! `/* unsupported expr type 1 */` shader body) and no 64-bit boolean (a
  ! logical(8) was lowered to a 4-byte `int`).  Both must keep the
  ! `do concurrent` on the CPU.
  implicit none

  complex(8), allocatable :: z8(:)
  complex, allocatable :: z4(:)
  logical(8), allocatable :: l(:)
  integer :: i

  allocate(z8(4))
  z8 = (0.0d0, 0.0d0)
  do concurrent (i = 1:4)
    z8(i) = (1.0d0, 2.0d0)
  end do
  if (abs(real(sum(z8)) - 4.0d0) > 1.0d-12) error stop "complex(8) real part"
  if (abs(aimag(sum(z8)) - 8.0d0) > 1.0d-12) error stop "complex(8) imag part"

  allocate(z4(4))
  z4 = (0.0, 0.0)
  do concurrent (i = 1:4)
    z4(i) = (3.0, -1.0)
  end do
  if (abs(real(sum(z4)) - 12.0) > 1.0e-5) error stop "complex(4) real part"
  if (abs(aimag(sum(z4)) + 4.0) > 1.0e-5) error stop "complex(4) imag part"

  allocate(l(4))
  l = .false.
  do concurrent (i = 1:4)
    l(i) = .true.
  end do
  if (count(l) /= 4) error stop "logical(8)"

  print *, "PASS"
end program gpu_metal_217
