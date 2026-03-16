program read_41
  ! List-directed internal read of complex with leading whitespace.
  ! The Fortran standard requires leading blanks to be ignored.
  implicit none
  complex :: z
  character(len=:), allocatable :: s

  ! No leading space
  s = "(1.0,0.0)"
  read(s, *) z
  if (abs(real(z) - 1.0) > 1e-6) error stop "Test 1 real part failed"
  if (abs(aimag(z) - 0.0) > 1e-6) error stop "Test 1 imag part failed"

  ! One leading space
  s = " (1.0,0.0)"
  read(s, *) z
  if (abs(real(z) - 1.0) > 1e-6) error stop "Test 2 real part failed"
  if (abs(aimag(z) - 0.0) > 1e-6) error stop "Test 2 imag part failed"

  ! Multiple leading spaces
  s = "   (2.5,-3.0)"
  read(s, *) z
  if (abs(real(z) - 2.5) > 1e-6) error stop "Test 3 real part failed"
  if (abs(aimag(z) - (-3.0)) > 1e-6) error stop "Test 3 imag part failed"

  print *, "All tests passed."
end program read_41
