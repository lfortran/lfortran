program intrinsics_207

    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32
    integer :: x
    real :: x4
    real(8) :: x8
    complex(8) :: z8
  
    print*, dacosh(1.0_dp)
    if( abs(dacosh(1.0_dp) - 0.0_dp) > 10e-5 ) error stop
  
    print*, dacos(1.0_dp)
    if( abs(dacos(1.0_dp) - 0.0_dp) > 10e-5 ) error stop
  
    print*, dasin(1.0_8)
    if (abs(dasin(1.0_8) - 1.570796326794896_sp) > 10e-5 ) error stop
  
    print*, dasinh(1.0_8)
    if (abs(dasinh(1.0_8) - 0.881373587019543_sp) > 10e-5 ) error stop
  
    print*, datanh(0.0_dp)
    if (abs(datanh(0.0_dp) - 0.0_dp) > 10e-5 ) error stop
  
    print*, isign(1, 1)
    if (isign(1, 1) /= 1) error stop
  
    print*, alog(1.0)
    if (abs(alog(1.0) - 0.0) > 10e-5 ) error stop
  
    print*, alog10(1.0)
    if (abs(alog10(1.0) - 0.0) > 10e-5 ) error stop
  
    print*, imagpart((1.0_8, 2.0_8))
    if (abs(imagpart((1.0_8, 2.0_8)) - 2.0_8) > 10e-5 ) error stop
  
    print*, dint(1.0_8)
    if (abs(dint(1.0_8) - 1.0_8) > 10e-5 ) error stop
  
    print*, dnint(1.0_8)
    if (abs(dnint(1.0_8) - 1.0_8) > 10e-5 ) error stop
  
    print*, dbesj0(1.0_8)
    if (abs(dbesj0(1.0_8) - 0.765197686557967_sp) > 10e-5 ) error stop
  
    print*, dbesj1(1.0_8)
    if (abs(dbesj1(1.0_8) - 0.440050585744933_sp) > 10e-5 ) error stop
  
    print*, dbesy0(1.0_8)
    if (abs(dbesy0(1.0_8) - 0.088256964215676_sp) > 10e-5 ) error stop
  
    print*, dbesy1(1.0_8)
    if (abs(dbesy1(1.0_8) - (-7.81212821300288685e-01_sp)) > 10e-5 ) error stop
  
    print*, idim(1, 1)
    if (idim(1, 1) /= 0) error stop
  
    print*, ddim(1.0_8, 1.0_8)
    if (abs(ddim(1.0_8, 1.0_8) - 0.0_8) > 10e-5 ) error stop
  
    print*, dconjg((1.0_8, 2.0_8))
    if (abs(dconjg((1.0_8, 2.0_8)) - (1.0_8, -2.0_8)) > 10e-5 ) error stop
  
    print*, amod(1.0, 1.0)
    if (amod(1.0, 1.0) /= 0.0) error stop
  
    print*, dmod(1.0_8, 1.0_8)
    if (abs(dmod(1.0_8, 1.0_8) - 0.0_8) > 10e-5 ) error stop
  
    x4 = 1.0
    x8 = 1.0_dp
    print*, dacosh(x8)
    if (abs(dacosh(x8) - 0.0) > 10e-5 ) error stop

    print*, dacos(x8)
    if (abs(dacos(x8) - 0.0_dp) > 10e-5 ) error stop

    print*, dasin(x8)
    if (abs(dasin(x8) - 1.570796326794896_sp) > 10e-5 ) error stop

    print*, dasinh(x8)
    if (abs(dasinh(x8) - 0.881373587019543_sp) > 10e-5 ) error stop

    x8 = 0.0_dp
    print*, datanh(x8)
    if (abs(datanh(x8) - 0.0_sp) > 10e-5 ) error stop

    x = 1
    print*, isign(x, x)
    if (isign(x, x) /= 1) error stop

    print*, alog(x4)
    if (abs(alog(x4) - 0.0) > 10e-5 ) error stop

    print*, alog10(x4)
    if (abs(alog10(x4) - 0.0) > 10e-5 ) error stop

    z8 = (1.0_dp, 2.0_dp)
    print*, imagpart(z8)
    if (abs(imagpart(z8) - 2.0) > 10e-5 ) error stop

    x8 = 1.0_dp
    print*, dint(x8)
    if (abs(dint(x8) - 1.0) > 10e-5 ) error stop

    print*, dnint(x8)
    if (abs(dnint(x8) - 1.0) > 10e-5 ) error stop

    print*, dbesj0(x8)
    if (abs(dbesj0(x8) - 0.765197686557967) > 10e-5 ) error stop

    print*, dbesj1(x8)
    if (abs(dbesj1(x8) - 0.440050585744933) > 10e-5 ) error stop

    print*, dbesy0(x8)
    if (abs(dbesy0(x8) - 0.088256964215676) > 10e-5 ) error stop

    print*, dbesy1(x8)
    if (abs(dbesy1(x8) - (-7.81212821300288685e-01)) > 10e-5 ) error stop

    print*, idim(x, x)
    if (idim(x, x) /= 0) error stop

    print*, ddim(x8, x8)
    if (abs(ddim(x8, x8) - 0.0) > 10e-5 ) error stop

    print*, dconjg(z8)
    if (abs(dconjg(z8) - (1.0_dp, -2.0_dp)) > 10e-5 ) error stop

    print*, amod(x4, x4)
    if (amod(x4, x4) /= 0.0) error stop

    print*, dmod(x8, x8)
    if (abs(dmod(x8, x8) - 0.0) > 10e-5 ) error stop

end program 