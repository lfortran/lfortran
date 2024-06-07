program intrinsics_245
    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32
    real(dp) :: a, b
    real(sp) :: e, f

    a = 79.72827_dp
    b = 43.91820_dp

    e = 81.6271
    f = 45.9182

    print *, besJ1(a)
    if(abs(besJ1(a) - (-3.5436241532298428E-002)) > 1e12_dp) error stop

    print *, besJ1(b)
    if(abs(besJ1(b) - (-8.9738951982497303E-002)) > 1e12_dp) error stop

    print *, besJ1(e)
    if(abs(besJ1(e) - (-6.54745325E-02)) > 1e5_sp) error stop

    print *, besJ1(f)
    if(abs(besJ1(f) - 0.107897274) > 1e5_sp) error stop

    print *, besJ0(a)
    if(abs(besJ0(a) - (-8.2251582251227029E-002)) > 1e12_dp) error stop

    print *, besJ0(b)
    if(abs(besJ0(b) - 7.9245871249582836E-002) > 1e12_dp) error stop

    print *, besJ0(e)
    if(abs(besJ0(e) - 5.88632450E-02) > 1e5_sp) error stop

    print *, besJ0(f)
    if(abs(besJ0(f) - 4.83344756E-02) > 1e5_sp) error stop

end program 