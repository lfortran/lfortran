program complex_08
    use iso_fortran_env, only: sp => real32, dp => real64
    implicit none
    complex(sp), parameter :: c1 = conjg((1.0_sp, 2.0_sp))
    complex(dp), parameter :: c2 = conjg((-3.0_dp, -4.0_dp))
    complex(sp), parameter :: ar1(2) = conjg([(1.0_sp, 2.0_sp), (-3.0_sp, 4.0_sp)])
    complex(dp), parameter :: ar2(2) = conjg([(-1.0_dp, -2.0_dp), (3.0_dp, -4.0_dp)])

    complex(sp) :: x = conjg((0, 0))
    complex(dp) :: y = conjg((2.0, -5.5))
    complex(dp) :: z = (42, 3.14)
    complex(sp) :: arr1(3) = [(0, 0), (2.0, 5.5), (42, -3.14)]
    complex(dp) :: arr2(3) = [(-1.0, -2.0), (3.0, -4.0), (42, -3.14)]

    print *, c1 
    if (abs(c1 - (1.0_sp, -2.0_sp)) > 1e-5) error stop
    print *, c2
    if (abs(c2 - (-3.0_dp, 4.0_dp)) > 1e-5) error stop
    print *, ar1
    if (any(abs(ar1 - [(1.0_sp, -2.0_sp), (-3.0_sp, -4.0_sp)]) > 1e-5)) error stop
    print *, ar2
    if (any(abs(ar2 - [(-1.0_dp, 2.0_dp), (3.0_dp, 4.0_dp)]) > 1e-5)) error stop

    print *, x
    if (abs(x - (0, 0)) > 1e-5) error stop
    print *, y
    if (abs(y - (2.0, 5.5)) > 1e-5) error stop
    print *, conjg(z)
    if (abs(conjg(z) - (42, -3.14)) > 1e-5) error stop
    
    print *, conjg(arr1)
    if (any(abs(conjg(arr1) - [(0, 0), (2.0, -5.5), (42, 3.14)]) > 1e-5)) error stop
    print *, conjg(arr2)
    if (any(abs(conjg(arr2) - [(-1.0, 2.0), (3.0, 4.0), (42, 3.14)]) > 1e-5)) error stop

end program
