program intrinsics_395
  implicit none
  complex(8) :: a
  a = cmplx(cmplx(1, 2), kind=8)
  if (abs(real(a, kind=8) - 1.0_8) > 1.0e-9_8) error stop
  if (abs(aimag(a) - 2.0_8) > 1.0e-9_8) error stop
end program intrinsics_395
