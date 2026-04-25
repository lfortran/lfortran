program equivalence_31
  implicit none

  character(40) :: cdata
  character(4) :: ans
  real :: r1(2)
  complex :: c1
  equivalence (r1(1), c1)

  cdata = '   2.343   34.394'
  READ (cdata, 100) c1
100 FORMAT (F10.5,1X,F10.5)
    ans = merge ('pass', 'fail',  &
    abs ( real (c1) -  2.343) < 0.001 .and.  &
    abs (aimag (c1) - 34.394) < 0.001)
    if( ans /= 'pass' ) error stop
end program equivalence_31