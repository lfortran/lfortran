program intrinsics_266
    use iso_fortran_env, only: sp => real32, dp => real64
    implicit none
    real(sp), parameter :: a1 = tan(1.0_sp)
    real(dp), parameter :: a2 = tan(1.0_dp) 
    complex(sp), parameter :: a3 = tan((1.0_sp, 1.5_sp))
    complex(dp), parameter :: a4 = tan((1.0_dp, 1.5_dp))

    real(sp), parameter :: ar1(3) = tan([1.0_sp, 1.5_sp, 2.0_sp])
    real(dp), parameter :: ar2(3) = tan([1.0_dp, 1.5_dp, 2.0_dp])
    complex(sp), parameter :: ac1(3) = tan([(1.0_sp, 1.5_sp), (2.0_sp, 2.5_sp), (3.0_sp, 3.5_sp)])
    complex(dp), parameter :: ac2(3) = tan([(1.0_dp, 1.5_dp), (2.0_dp, 2.5_dp), (3.0_dp, 3.5_dp)])

    real(sp) :: b1 = 0.5_sp
    real(dp) :: b2 = 0.7_dp
    complex(sp) :: b3 = (0.5_sp, 0.7_sp)
    complex(dp) :: b4 = (0.5_dp, 0.7_dp)

    real(sp) :: br1(3) = [0.5_sp, 0.7_sp, 0.9_sp]
    real(dp) :: br2(3) = [0.5_dp, 0.7_dp, 0.9_dp]
    complex(sp) :: bc1(3) = [(0.5_sp, 0.7_sp), (0.9_sp, 1.1_sp), (1.3_sp, 1.5_sp)]
    complex(dp) :: bc2(3) = [(0.5_dp, 0.7_dp), (0.9_dp, 1.1_dp), (1.3_dp, 1.5_dp)]

    print *, a1
    if (abs(a1 - 1.55740772e+00_sp) > 1e-6_sp) error stop
    print *, a2
    if (abs(a2 - 1.55740772465490223_dp) > 1e-12_dp) error stop
    print *, a3
    if (abs(a3 - (9.421291947E-02_sp, 1.03795874_sp)) > 1e-6_sp) error stop
    print *, a4
    if (abs(a4 - (9.42129201295443947E-002_dp, 1.0379587828579324_dp)) > 1e-12_dp) error stop

    print *, ar1
    if (any(abs(ar1 - [1.55740772_sp, 14.1014194_sp, -2.18503976_sp]) > 1e-6_sp)) error stop
    print *, ar2
    if (any(abs(ar2 - [1.55740772465490223_dp, 14.101419947171719_dp, -2.185039863261519_dp]) > 1e-12_dp)) error stop
    print *, ac1
    if (any(abs(ac1 - [(9.421291947E-02_sp, 1.03795874_sp), (-1.028875075E-02_sp, 1.00879467_sp), &
        (-5.086967140E-04_sp, 0.998250306_sp)]) > 1e-6_sp)) error stop
    print *, ac2
    if (any(abs(ac2 - [(9.42129201295443947E-002_dp, 1.0379587828579324_dp), (-1.02887508595820281E-002_dp, &
        1.0087947005319189_dp), (-5.08696693455815119E-004_dp, 0.99825027844029535_dp)]) > 1e-12_dp)) error stop

    print *, tan(b1)
    if (abs(tan(b1) - 0.546302497_sp) > 1e-6_sp) error stop
    print *, tan(b2)
    if (abs(tan(b2) - 0.84228838046307941_dp) > 1e-12_dp) error stop
    print *, tan(b3)
    if (abs(tan(b3) - (0.312674940_sp, 0.707602859_sp)) > 1e-6_sp) error stop
    print *, tan(b4)
    if (abs(tan(b4) - (0.31267491960977922_dp, 0.70760291160255895_dp)) > 1e-12_dp) error stop

    print *, tan(br1)
    if (any(abs(tan(br1) - [0.546302497_sp, 0.842288375_sp, 1.26015818_sp]) > 1e-6_sp)) error stop
    print *, tan(br2)
    if (any(abs(tan(br2) - [0.54630248984379048_dp, 0.84228838046307941_dp, 1.2601582175503392_dp]) > 1e-12_dp)) error stop
    print *, tan(bc1)
    if (any(abs(tan(bc1) - [(0.312674940_sp, 0.707602859_sp), (0.224352330_sp, 1.02681565_sp), &
        (5.596723780E-02_sp, 1.08762586_sp)]) > 1e-6_sp)) error stop
    print *, tan(bc2)
    if (any(abs(tan(bc2) - [(0.31267491960977922_dp, 0.70760291160255895_dp), (0.22435234690709807_dp, 1.0268156677854674_dp), &
        (5.59672199341484863E-002_dp, 1.0876258337665916_dp)]) > 1e-12_dp)) error stop
    
end program