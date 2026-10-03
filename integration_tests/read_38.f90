program read_38
  implicit none
  character(len=20) :: s1, s2
  complex :: c32
  complex(8) :: c64

  s1 = "(1.0, 2.0)"
  s2 = "(3.0d0, 4.0d0)"

  read(s1, *) c32
  read(s2, *) c64

  if (abs(real(c32) - 1.0) > 1e-6) error stop
  if (abs(aimag(c32) - 2.0) > 1e-6) error stop
  if (abs(real(c64) - 3.0d0) > 1d-12) error stop
  if (abs(aimag(c64) - 4.0d0) > 1d-12) error stop

  print *, "OK"
end program
