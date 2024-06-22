program intrinsics_265
    use iso_fortran_env, only: sp => real32, dp => real64
    implicit none
    real(sp), parameter :: a1 = cos(1.0_sp)
    real(dp), parameter :: a2 = cos(1.0_dp) 
    complex(sp), parameter :: a3 = cos((1.0_sp, 1.5_sp))
    complex(dp), parameter :: a4 = cos((1.0_dp, 1.5_dp))

    real(sp), parameter :: ar1(3) = cos([1.0_sp, 1.5_sp, 2.0_sp])
    real(dp), parameter :: ar2(3) = cos([1.0_dp, 1.5_dp, 2.0_dp])
    complex(sp), parameter :: ac1(3) = cos([(1.0_sp, 1.5_sp), (2.0_sp, 2.5_sp), (3.0_sp, 3.5_sp)])
    complex(dp), parameter :: ac2(3) = cos([(1.0_dp, 1.5_dp), (2.0_dp, 2.5_dp), (3.0_dp, 3.5_dp)])

    real(sp) :: b1 = 0.5_sp
    real(dp) :: b2 = 0.7_dp
    complex(sp) :: b3 = (0.5_sp, 0.7_sp)
    complex(dp) :: b4 = (0.5_dp, 0.7_dp)

    real(sp) :: br1(3) = [0.5_sp, 0.7_sp, 0.9_sp]
    real(dp) :: br2(3) = [0.5_dp, 0.7_dp, 0.9_dp]
    complex(sp) :: bc1(3) = [(0.5_sp, 0.7_sp), (0.9_sp, 1.1_sp), (1.3_sp, 1.5_sp)]
    complex(dp) :: bc2(3) = [(0.5_dp, 0.7_dp), (0.9_dp, 1.1_dp), (1.3_dp, 1.5_dp)]

    print *, a1
    if (abs(a1 - 0.540302277_sp) > 1e-6_sp) error stop
    print *, a2
    if (abs(a2 - 0.54030230586813977_dp) > 1e-12_dp) error stop
    print *, a3
    if (abs(a3 - (1.27101231_sp, -1.79172683_sp)) > 1e-6_sp) error stop
    print *, a4
    if (abs(a4 - (1.2710123394623098_dp, -1.7917268800098574_dp)) > 1e-12_dp) error stop

    print *, ar1
    if (any(abs(ar1 - [0.540302277_sp, 0.0707372017_sp, -0.416146845_sp]) > 1e-6_sp)) error stop
    print *, ar2
    if (any(abs(ar2 - [0.54030230586813977_dp, 0.070737201667702906_dp, -0.41614683654714241_dp]) > 1e-12_dp)) error stop
    print *, ac1
    if (any(abs(ac1 - [(1.27101231_sp, -1.79172683_sp), (-2.55193281_sp, -5.50143528_sp), &
        (-16.4069729_sp, -2.33449578_sp)]) > 1e-6_sp)) error stop
    print *, ac2
    if (any(abs(ac2 - [(1.2710123394623098_dp, -1.7917268800098574_dp), (-2.5519328677533650_dp, -5.5014353663786872_dp), &
        (-16.406972071821489_dp, -2.3344956961624304_dp)]) > 1e-12_dp)) error stop

    print *, cos(b1)
    if (abs(cos(b1) - 0.877582550_sp) > 1e-6_sp) error stop
    print *, cos(b2)
    if (abs(cos(b2) - 0.76484218728448850_dp) > 1e-12_dp) error stop
    print *, cos(b3)
    if (abs(cos(b3) - (1.10151446_sp, -0.363684386_sp)) > 1e-6_sp) error stop
    print *, cos(b4)
    if (abs(cos(b4) - (1.1015144315669947_dp, -0.36368439983078843_dp)) > 1e-12_dp) error stop

    print *, cos(br1)
    if (any(abs(cos(br1) - [0.877582550_sp, 0.764842212_sp, 0.621609986_sp]) > 1e-6_sp)) error stop
    print *, cos(br2)
    if (any(abs(cos(br2) - [0.87758256189037276_dp, 0.76484218728448850_dp, 0.62160996827066439_dp]) > 1e-12_dp)) error stop
    print *, cos(bc1)
    if (any(abs(cos(bc1) - [(1.10151446_sp, -0.363684386_sp), (1.03716779_sp, -1.04624867_sp), &
        (0.629266918_sp, -2.05168462_sp)]) > 1e-6_sp)) error stop
    print *, cos(bc2)
    if (any(abs(cos(bc2) - [(1.1015144315669947_dp, -0.36368439983078843_dp), (1.0371677653004676_dp, &
        -1.0462486051241380_dp), (0.62926681652278482_dp, -2.0516846479972717_dp)]) > 1e-12_dp)) error stop
    
end program