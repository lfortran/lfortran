program intrinsics_212
    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32
    integer :: x = 1
    complex(4) :: z4
    complex(8) :: z8

    print*, cabs((1.0, 2.0))
    if (abs(cabs((1.0, 2.0)) - 2.23606798) > 10e-5) error stop
    print*, zabs((1.0_8, 2.0_8))
    if (abs(zabs((1.0_8, 2.0_8)) - 2.23606798_8) > 10e-5) error stop
    print*, cdabs((1.0_8, 2.0_8))
    if (abs(cdabs((1.0_8, 2.0_8)) - 2.23606798_8) > 10e-5) error stop

    print*, csin((1.0, 2.0)) 
    if (abs(csin((1.0, 2.0)) - (3.16577840,1.95960104)) > 10e-5) error stop 
    print*, zsin((1.0_8, 2.0_8)) 
    if (abs(zsin((1.0_8, 2.0_8)) - (3.16577840_8,1.95960104_8)) > 10e-5) error stop 
    print*, cdsin((1.0_8, 2.0_8)) 
    if (abs(cdsin((1.0_8, 2.0_8)) - (3.16577840_8,1.95960104_8)) > 10e-5) error stop

    print*, ccos((1.0, 2.0)) 
    if (abs(ccos((1.0, 2.0)) - (2.03272295,-3.05189776)) > 10e-5) error stop 
    print*, zcos((1.0_8, 2.0_8)) 
    if (abs(zcos((1.0_8, 2.0_8)) - (2.03272295_8,-3.05189776_8)) > 10e-5) error stop 
    print*, cdcos((1.0_8, 2.0_8)) 
    if (abs(cdcos((1.0_8, 2.0_8)) - (2.03272295_8,-3.05189776_8)) > 10e-5) error stop

    print*, csqrt((1.0, 2.0)) 
    if (abs(csqrt((1.0, 2.0)) - (1.27201965,0.78615138)) > 10e-5) error stop 
    print*, zsqrt((1.0_8, 2.0_8)) 
    if (abs(zsqrt((1.0_8, 2.0_8)) - (1.27201965_8,0.78615138_8)) > 10e-5) error stop 
    print*, cdsqrt((1.0_8, 2.0_8)) 
    if (abs(cdsqrt((1.0_8, 2.0_8)) - (1.27201965_8,0.78615138_8)) > 10e-5) error stop

    print*, cexp((1.0, 2.0)) 
    if (abs(cexp((1.0, 2.0)) - (-1.13120438,2.47172667)) > 10e-5) error stop 
    print*, zexp((1.0_8, 2.0_8)) 
    if (abs(zexp((1.0_8, 2.0_8)) - (-1.13120438_8,2.47172667_8)) > 10e-5) error stop 
    print*, cdexp((1.0_8, 2.0_8)) 
    if (abs(cdexp((1.0_8, 2.0_8)) - (-1.13120438_8,2.47172667_8)) > 10e-5) error stop

    print*, xor(1, 1) 
    if (xor(1, 1) /= 0) error stop

    z4 = (1.0, 2.0)
    z8 = (1.0_dp, 2.0_dp)

    print*, cabs(z4)
    if (abs(cabs(z4) - 2.23606798) > 10e-5) error stop
    print*, zabs(z8)
    if (abs(zabs(z8) - 2.23606798_8) > 10e-5) error stop
    print*, cdabs(z8)
    if (abs(cdabs(z8) - 2.23606798_8) > 10e-5) error stop

    print*, csin(z4) 
    if (abs(csin(z4) - (3.16577840,1.95960104)) > 10e-5) error stop 
    print*, zsin(z8) 
    if (abs(zsin(z8) - (3.16577840_8,1.95960104_8)) > 10e-5) error stop 
    print*, cdsin(z8) 
    if (abs(cdsin(z8) - (3.16577840_8,1.95960104_8)) > 10e-5) error stop

    print*, ccos(z4) 
    if (abs(ccos(z4) - (2.03272295,-3.05189776)) > 10e-5) error stop 
    print*, zcos(z8) 
    if (abs(zcos(z8) - (2.03272295_8,-3.05189776_8)) > 10e-5) error stop 
    print*, cdcos(z8) 
    if (abs(cdcos(z8) - (2.03272295_8,-3.05189776_8)) > 10e-5) error stop

    print*, csqrt(z4) 
    if (abs(csqrt(z4) - (1.27201965,0.78615138)) > 10e-5) error stop 
    print*, zsqrt(z8) 
    if (abs(zsqrt(z8) - (1.27201965_8,0.78615138_8)) > 10e-5) error stop 
    print*, cdsqrt(z8) 
    if (abs(cdsqrt(z8) - (1.27201965_8,0.78615138_8)) > 10e-5) error stop

    print*, cexp(z4) 
    if (abs(cexp(z4) - (-1.13120438,2.47172667)) > 10e-5) error stop 
    print*, zexp(z8) 
    if (abs(zexp(z8) - (-1.13120438_8,2.47172667_8)) > 10e-5) error stop 
    print*, cdexp(z8) 
    if (abs(cdexp(z8) - (-1.13120438_8,2.47172667_8)) > 10e-5) error stop

    print*, xor(x, x) 
    if (xor(x, x) /= 0) error stop

end program