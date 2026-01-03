program intrinsics_264
    use iso_fortran_env, only: sp => real32, dp => real64
    implicit none
    real(sp), parameter :: a1 = sin(1.0_sp)
    real(dp), parameter :: a2 = sin(1.0_dp) 
    complex(sp), parameter :: a3 = sin((1.0_sp, 1.5_sp))
    complex(dp), parameter :: a4 = sin((1.0_dp, 1.5_dp))

    real(sp), parameter :: ar1(3) = sin([1.0_sp, 1.5_sp, 2.0_sp])
    real(dp), parameter :: ar2(3) = sin([1.0_dp, 1.5_dp, 2.0_dp])
    complex(sp), parameter :: ac1(3) = sin([(1.0_sp, 1.5_sp), (2.0_sp, 2.5_sp), (3.0_sp, 3.5_sp)])
    complex(dp), parameter :: ac2(3) = sin([(1.0_dp, 1.5_dp), (2.0_dp, 2.5_dp), (3.0_dp, 3.5_dp)])

    real(sp) :: b1 = 0.5_sp
    real(dp) :: b2 = 0.7_dp
    complex(sp) :: b3 = (0.5_sp, 0.7_sp)
    complex(dp) :: b4 = (0.5_dp, 0.7_dp)

    real(sp) :: br1(3) = [0.5_sp, 0.7_sp, 0.9_sp]
    real(dp) :: br2(3) = [0.5_dp, 0.7_dp, 0.9_dp]
    complex(sp) :: bc1(3) = [(0.5_sp, 0.7_sp), (0.9_sp, 1.1_sp), (1.3_sp, 1.5_sp)]
    complex(dp) :: bc2(3) = [(0.5_dp, 0.7_dp), (0.9_dp, 1.1_dp), (1.3_dp, 1.5_dp)]

    print *, a1
    if (abs(a1 - 8.41470957e-01_sp) > 1e-6_sp) error stop
    print *, a2
    if (abs(a2 - 8.41470984807896507e-01_dp) > 1e-12_dp) error stop
    print *, a3
    if (abs(a3 - (1.979484_sp, 1.150455_sp)) > 1e-6_sp) error stop
    print *, a4
    if (abs(a4 - (1.9794844356103003_dp, 1.1504545994253859_dp)) > 1e-12_dp) error stop

    print *, ar1
    if (any(abs(ar1 - [0.841470957_sp, 0.997494996_sp, 0.909297407_sp]) > 1e-6_sp)) error stop
    print *, ar2
    if (any(abs(ar2 - [0.84147098480789650_dp, 0.99749498660405445_dp, 0.90929742682568171_dp]) > 1e-12_dp)) error stop
    print *, ac1
    if (any(abs(ac1 - [(1.97948444_sp, 1.15045464_sp), (5.57607508_sp, -2.51777339_sp), &
        (2.33875704_sp, -16.3770771_sp)]) > 1e-6_sp)) error stop
    print *, ac2
    if (any(abs(ac2 - [(1.9794844356103003_dp, 1.1504545994253859_dp), (5.5760750444083884_dp, -2.5177734552480526_dp), &
        (2.3387571511543750_dp, -16.377076888816426_dp)]) > 1e-12_dp)) error stop

    print *, sin(b1)
    if (abs(sin(b1) - 0.479425550_sp) > 1e-6_sp) error stop
    print *, sin(b2)
    if (abs(sin(b2) - 0.64421768723769102_dp) > 1e-12_dp) error stop
    print *, sin(b3)
    if (abs(sin(b3) - (0.601760089_sp, 0.665719807_sp)) > 1e-6_sp) error stop
    print *, sin(b4)
    if (abs(sin(b4) - (0.60176007656391672_dp, 0.66571982846862043_dp)) > 1e-12_dp) error stop

    print *, sin(br1)
    if (any(abs(sin(br1) - [0.479425550_sp, 0.644217670_sp, 0.783326924_sp]) > 1e-6_sp)) error stop
    print *, sin(br2)
    if (any(abs(sin(br2) - [0.47942553860420301_dp, 0.64421768723769102_dp, 0.78332690962748341_dp]) > 1e-12_dp)) error stop
    print *, sin(bc1)
    if (any(abs(sin(bc1) - [(0.601760089_sp, 0.665719807_sp), (1.30699551_sp, 0.830251873_sp), &
        (2.26668358_sp, 0.569579840_sp)]) > 1e-6_sp)) error stop
    print *, sin(bc2)
    if (any(abs(sin(bc2) - [(0.60176007656391672_dp, 0.66571982846862043_dp), (1.3069954824217060_dp, 0.83025178152468282_dp), &
        (2.2666835402217402_dp, 0.56957976005226330_dp)]) > 1e-12_dp)) error stop
    
end program