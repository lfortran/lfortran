program intrinsics_255
    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32
    
    print*, dbesjn(0, 2.0_dp)
    if (abs(dbesjn(0, 2.0_dp) - 0.22389077914123567_dp) > 10e-5_dp) error stop 

    print *, dbesjn(1, 1.0_dp)
    if (abs(dbesjn(1, 1.0_dp) - 0.440050585744933_dp) > 10e-5_dp) error stop

    print *, dbesjn(2, 7.0_dp)
    if (abs(dbesjn(2, 7.0_dp) - (-0.30141722008594013_dp)) > 10e-5_dp) error stop

    print *, dbesjn(3, 0.5_dp)
    if (abs(dbesjn(3, 0.5_dp) - 2.5637299945872440E-003_dp) > 10e-5_dp) error stop

    print *, dbesjn(4, -13.6_dp)
    if (abs(dbesjn(4, -13.6_dp) - 0.14930687731069392_dp) > 10e-5_dp) error stop

    print *, dbesyn(0, 2.0_dp)
    if (abs(dbesyn(0, 2.0_dp) - 0.510375672649745_dp) > 10e-5_dp) error stop

    print *, dbesyn(1, 1.0_dp)
    if (abs(dbesyn(1, 1.0_dp) - (-0.7812128213002889_dp)) > 10e-5_dp) error stop

    print *, dbesyn(2, 7.0_dp)
    if (abs(dbesyn(2, 7.0_dp) - (-6.0526609468272125E-002_dp)) > 10e-5_dp) error stop

    print *, dbesyn(3, 0.5_dp)
    if (abs(dbesyn(3, 0.5_dp) - (-42.0594943047238914_dp)) > 10e-5_dp) error stop

    print *, dbesyn(4, 10.0_dp)
    if (abs(dbesyn(4, 10.0_dp) - (-1.44949511868093739e-01_dp)) > 10e-5_dp) error stop

end program