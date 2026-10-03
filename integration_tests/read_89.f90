program read_89
  implicit none

  character(40) :: cdata
  real :: r1(2)
  complex :: c1
  equivalence (r1(1), c1)

  cdata = '   2.343   34.394'
  READ (cdata, 100) c1
100 FORMAT (F10.5,1X,F10.5)

  ! ---- added checks ----
  if (abs(real(c1) - 2.343) >= 0.001) error stop "incorrect real part"
  if (abs(aimag(c1) - 34.394) >= 0.001) error stop "incorrect imaginary part"

  print *, 'c1 =', c1
  print*, "test pass"

end program read_89