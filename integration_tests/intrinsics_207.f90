program intrinsics_207

    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32
  
    print*, dacosh(1.0_dp)
    if( abs(dacosh(1.0_dp) - 0.0_dp) > 10e-5 ) error stop
  
    print*, dacos(1.0_dp)
    if( abs(dacos(1.0_dp) - 0.0_dp) > 10e-5 ) error stop
  
    print*, dasin(1.0_8)
    if (abs(dasin(1.0_8) - 1.570796326794896_sp) > 10e-5 ) error stop
  
    print*, dasinh(1.0_8)
    if (abs(dasinh(1.0_8) - 0.881373587019543_sp) > 10e-5 ) error stop
  
    print*, datanh(0.8_8)
    if (abs(datanh(0.8_8) - 1.0986122886681098_sp) > 10e-5 ) error stop
  
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
  
end program 
  