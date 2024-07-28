program intrinsics_295
    use iso_fortran_env, only: sp=>real32, dp=>real64
    implicit none
    real(sp) :: x
    real(dp) :: y
    complex(sp) :: z
    complex(dp) :: w
    real(sp), parameter :: x1 = exp(1.5_sp)
    real(dp), parameter :: y1 = exp(1.5_dp)
    complex(sp), parameter :: x2 = exp((1.5_sp, 2.5_sp))
    complex(dp), parameter :: y2 = exp((1.11_dp, 2.21_dp))
    real(sp), parameter :: ar1(3) = exp([1.1_sp, 2.2_sp, 3.3_sp])
    real(dp), parameter :: ar2(3) = exp([1.31_dp, 2.42_dp, 3.13_dp])
    complex(sp), parameter :: ar3(3) = exp([(1.5_sp, 2.5_sp), (3.5_sp, 4.5_sp), (5.5_sp, 6.5_sp)])
    complex(dp), parameter :: ar4(3) = exp([(1.5_dp, 2.5_dp), (3.5_dp, 4.5_dp), (5.5_dp, 6.5_dp)])
    real(sp) :: arr1(3) = [1.1_sp, 2.2_sp, 3.3_sp]
    real(dp) :: arr2(3) = [1.31_dp, 2.42_dp, 3.13_dp]
    complex(sp) :: arr3(3) = [(1.1_sp, 2.2_sp), (3.3_sp, 4.4_sp), (5.5_sp, 6.6_sp)]
    complex(dp) :: arr4(3) = [(1.1_dp, 2.2_dp), (3.3_dp, 4.4_dp), (5.5_dp, 6.6_dp)]
    complex(sp) :: res1(3)
    complex(dp) :: res2(3)
    
    print *, x1
    if (abs(x1 - 4.48168907_sp) > 1e-5) error stop
    print *, y1
    if (abs(y1 - 4.481689070338064822e+00_dp) > 1e-10) error stop
    print *, x2
    if (abs(x2 - (-3.59047651_sp, 2.682166_sp)) > 1e-5) error stop
    print *, y2
    if (abs(y2 - (-1.81016628947939662e+00_dp, 2.4352882519970085_dp)) > 1e-10) error stop
    
    print *, ar1
    if (any(ar1 - [3.00416613e+00_sp, 9.02501392e+00_sp, 2.71126385e+01_sp] > 1e-6)) error stop
    print *, ar2
    if (any(ar2 - [3.70617371221019898e+00_dp, 1.12458593148818444e+01_dp, 2.28739795424408072e+01_dp] > 1e-10)) error stop
    print *, ar3
    if (abs(ar3(1) - (-3.590477_sp,2.682166_sp)) > 1e-6) error stop
    if (abs(ar3(2) - (-6.980598_sp,-32.371353_sp)) > 1e-6) error stop
    if (abs(ar3(3) - (238.963120_sp,52.638126_sp) ) > 1e-6) error stop
    print *, ar4
    if (abs(ar4(1) - (-3.5904765855678136_dp, 2.6821660671324890_dp)) > 1e-10) error stop
    if (abs(ar4(2) - (-6.9805981691441259_dp, -32.371351649713368_dp)) > 1e-10) error stop
    if (abs(ar4(3) - (238.96311316471733_dp, 52.638125553863652_dp) ) > 1e-10) error stop
    
    x = exp(1.5_sp)
    print *, x
    if (abs(x - 4.48168907_sp) > 1e-5) error stop
    y = exp(1.5_dp)
    print *, y
    if (abs(y - 4.481689070338064822_dp) > 1e-10) error stop
    z = exp((1.5_sp, 2.5_sp))
    print *, z
    if (abs(z - ((-3.59047651_sp, 2.682166_sp))) > 1e-5) error stop
    w = exp((1.11_dp, 2.21_dp))
    print *, w
    if (abs(w - (-1.8101662894793968_dp, 2.4352882519970085_dp)) > 1e-10) error stop
    
    print *, exp(arr1)
    if (any(exp(arr1) - [3.00416613e+00_sp, 9.02501392e+00_sp, 2.71126385e+01_sp] > 1e-6)) error stop
    print *, exp(arr2)
    if (any(exp(arr2) - [3.7061737122101990_dp, 11.245859314881844_dp, 22.873979542440807_dp] > 1e-10)) error stop
    
    res1 = exp(arr3)
    print *, res1
    if (abs(res1(1) - (-1.767955_sp, 2.428857_sp)) > 1e-6) error stop
    if (abs(res1(2) - (-8.332603_sp, -25.800444_sp)) > 1e-6) error stop
    if (abs(res1(3) - (232.514252_sp, 76.231628_sp) ) > 1e-5) error stop
    
    res2 = exp(arr4)
    print *, res2
    if (abs(res2(1) - (-1.7679550615130259_dp, 2.4288574268376881_dp)) > 1e-10) error stop
    if (abs(res2(2) - (-8.3326051321743808_dp, -25.800443425515649_dp)) > 1e-10) error stop
    if (abs(res2(3) - (232.51424902677110_dp, 76.231658218318316_dp) ) > 1e-10) error stop  
  
end program
