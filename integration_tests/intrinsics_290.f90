program intrinsics_290
    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32
    real(dp) :: a, b
    real(sp) :: e, f

    a = 27.81425_dp
    b = 718.71820_dp

    e = 99.6271
    f = 19.7289

    print *, besY1(a)
    if (abs(besY1(a) - 5.0292223210638608E-002) > 1e12_dp) error stop

    print *, besY1(b)
    if (abs(besY1(b) - 2.3606808514659038E-003) > 1e12_dp) error stop

    print *, besY1(e)
    if (abs(besY1(e) - 9.14931018E-03) > 1e5_sp) error stop

    print *, besY1(f)
    if (abs(besY1(f) - (-0.178575486)) > 1e5_sp) error stop

    print *, besY0(a)
    if (abs(besY0(a) - 0.14355775840850599) > 1e12_dp) error stop

    print *, besY0(b)
    if (abs(besY0(b) - 2.9669766554274776E-002) > 1e12_dp) error stop

    print *, besY0(e)
    if (abs(besY0(e) - (-7.93649256E-02)) > 1e5_sp) error stop

    print *, besY0(f)
    if (abs(besY0(f) - 1.57167725E-02) > 1e5_sp) error stop

end program 