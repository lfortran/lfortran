program complex_25
  implicit none
  integer,parameter :: pr = 6, wp = selected_real_kind(pr)
  real(wp),parameter:: small = tiny(1.0_wp)
  complex(wp)       :: a, b
  a = cmplx(-2,1,wp)*small
  b = cmplx(-1,2,wp)*small
  print *, small
  if (abs(small - 1.1920929E-38_wp) > 1e-12_wp) error stop
  print *, a/b
  if (abs(a/b - (0.800000012,0.600000024)) > 1e-12_wp) error stop
end program complex_25
