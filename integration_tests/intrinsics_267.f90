program intrinsics_267
    use iso_fortran_env, only: sp => real32, dp => real64
    implicit none
    real(dp), parameter :: a1 = dsin(1.0_dp)
    real(dp), parameter :: a2 = dcos(1.0_dp) 
    real(dp), parameter :: a3 = dtan(1.0_dp)
    real(dp), parameter :: ar1(3) = dsin([1.0_dp, 1.5_dp, 2.0_dp])
    real(dp), parameter :: ar2(3) = dcos([1.0_dp, 1.5_dp, 2.0_dp])
    real(dp), parameter :: ar3(3) = dtan([1.0_dp, 1.5_dp, 2.0_dp])
    real(dp) :: b = 0.7_dp
    real(dp) :: br(3) = [0.5_dp, 0.7_dp, 0.9_dp]

    ! dsin 
    print *, a1
    if (abs(a1 - 8.41470984807896507e-01_dp) > 1e-12_dp) error stop
    print *, ar1
    if (any(abs(ar1 - [0.84147098480789650_dp, 0.99749498660405445_dp, 0.90929742682568171_dp]) > 1e-12_dp)) error stop
    print *, sin(b)
    if (abs(sin(b) - 0.64421768723769102_dp) > 1e-12_dp) error stop
    print *, sin(br)
    if (any(abs(sin(br) - [0.47942553860420301_dp, 0.64421768723769102_dp, 0.78332690962748341_dp]) > 1e-12_dp)) error stop

    ! dcos
    print *, a2
    if (abs(a2 - 5.40302305868139717e-01_dp) > 1e-12_dp) error stop
    print *, ar2
    if (any(abs(ar2 - [0.54030230586813977_dp, 0.070737201667702905_dp, -0.41614683654714241_dp]) > 1e-12_dp)) error stop
    print *, dcos(b)
    if (abs(dcos(b) - 0.76484218728448849_dp) > 1e-12_dp) error stop
    print *, dcos(br)
    if (any(abs(dcos(br) - [0.87758256189037276_dp, 0.76484218728448849_dp, 0.62160996827066441_dp]) > 1e-12_dp)) error stop
    
    ! dtan
    print *, a3
    if (abs(a3 - 1.55740772465490223_dp) > 1e-12_dp) error stop
    print *, ar3
    if (any(abs(ar3 - [1.55740772465490223_dp, 14.101419947171719_dp, -2.185039863261519_dp]) > 1e-12_dp)) error stop
    print *, dtan(b)
    if (abs(dtan(b) - 0.84228838046307941_dp) > 1e-12_dp) error stop
    print *, dtan(br)
    if (any(abs(dtan(br) - [0.54630248984379048_dp, 0.84228838046307941_dp, 1.2601582175503392_dp]) > 1e-12_dp)) error stop
    
end program