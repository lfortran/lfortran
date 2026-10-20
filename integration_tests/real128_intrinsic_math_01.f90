program real128_intrinsic_math_01
    ! Elemental math intrinsics of real(128) arguments known only at run
    ! time must be computed in quad precision.
    use, intrinsic :: iso_fortran_env, only: real128, int64
    implicit none
    integer, parameter :: qp = real128
    real(qp), parameter :: pi = 3.14159265358979323846264338327950288_qp
    real(qp) :: zero, one, two, x
    integer :: n

    ! Zero known only at run time, so that nothing is constant-folded.
    n = command_argument_count()
    zero = n
    one = 1 + zero
    two = 2 + zero

    ! The program from the issue.
    call check(acos(-one), pi, 1)
    call check(atan(one) * 4, pi, 2)
    call check(asin(1/two) * 6, pi, 3)
    call check(sqrt(two) ** 2, two, 4)
    call check(exp(log(two)), two, 5)

    call check(exp(two), 7.3890560989306502272304274605750080188_qp, 10)
    call check(exp(zero - 3.7_qp), 2.4723526470339391202757382983402626139e-2_qp, 11)
    call check(exp(zero + 50), 5.1847055285870724640874533229334856514e21_qp, 12)
    call check(exp(zero + 11000), 1.7350058192777385096384250773496418657e4777_qp, 13)
    call check(exp(zero - 11000), 5.7636694291681836962728229609612699850e-4778_qp, 14)

    call check(log(two), 6.9314718055994530941723212145817657508e-1_qp, 20)
    call check(log(zero + 7.3_qp), 1.9878743481543454450741174027541784146_qp, 21)
    call check(-log(zero + 1e-300_qp), 6.9077552789821370520539743640530929933e2_qp, 22)
    call check(-log(zero + 0.999_qp), 1.0005003335835335001429822540683264728e-3_qp, 23)
    ! log(2**13000)
    x = transfer([0_int64, 8270579240689205248_int64 + n], x)
    call check(log(x), 9.0109133472792890224240175789562948921e3_qp, 24)
    call check(log10(two), 3.0102999566398119521373889472449302020e-1_qp, 25)
    call check(log10(zero + 1000), 3 + zero, 26)

    call check(sin(two), 9.0929742682568169539601986591174486844e-1_qp, 30)
    call check(-sin(zero + 100), 5.0636564110975879365655761045978541936e-1_qp, 31)
    call check(-cos(zero + 3), 9.8999249660044545727157279473126133556e-1_qp, 32)
    call check(cos(zero + 1e6_qp), 9.3675212753314478693853253507491880983e-1_qp, 33)
    call check(-sin(zero + 1e30_qp), 9.0116901912138058030386428952987325866e-2_qp, 34)
    call check(tan(zero + 0.5_qp), 5.4630248984379051325517946578028535441e-1_qp, 35)
    call check(tan(zero + 1.5_qp), 1.4101419947171719387646083651987756857e1_qp, 36)
    call check(tan(zero + 1e3_qp), 1.4703241557027184459802088049039186453_qp, 37)

    call check(asin(zero + 0.9_qp), 1.1197695149986341866866770558453997516_qp, 40)
    call check(acos(1/two), 1.0471975511965977461542144610931676634_qp, 41)
    call check(acos(zero - 0.3_qp), 1.8754889808102941272033246528672805517_qp, 42)
    call check(acos(1 - 1e-20_qp + zero), 1.4142135623730980954348727132337478364e-10_qp, 43)
    call check(atan(zero + 3), 1.2490457723982544258299170772810900484_qp, 44)
    call check(atan(zero + 0.9_qp), 7.3281510178650659164079207273428024528e-1_qp, 45)
    call check(atan(zero + 1e-3_qp), 9.9999966666686666652380963492054386637e-4_qp, 46)
    call check(atan2(1/two, -two), 2.8966139904629290842905609020682270874_qp, 47)
    call check(-atan2(-one, -one), 2.3561944901923449288469825374596270981_qp, 48)

    call check(sinh(1/two), 5.2109530549374736162242562641149154621e-1_qp, 50)
    call check(sinh(zero + 10), 1.1013232874703393377236524554846364203e4_qp, 51)
    call check(sinh(zero + 1e-20_qp), 1.0000000000000000000000000000000000107e-20_qp, 52)
    call check(cosh(1/two), 1.1276259652063807852262251614026720302_qp, 53)
    call check(cosh(zero + 20), 2.4258259770489514001513022649004920024e8_qp, 54)
    call check(tanh(zero + 0.3_qp), 2.9131261245159090581822127282376593672e-1_qp, 55)
    call check(-tanh(-two), 9.6402758007581688394641372410092317105e-1_qp, 56)
    call check(asinh(zero + 1.5_qp), 1.1947632172871093041119308285190906083_qp, 57)
    call check(asinh(zero + 1e-10_qp), 9.9999999999999999999833333333333338711e-11_qp, 58)
    call check(acosh(zero + 2.5_qp), 1.5667992369724110786640568625804835797_qp, 59)
    call check(acosh(1 + 1e-10_qp + zero), 1.4142135623613099357821785970800893517e-5_qp, 60)
    call check(atanh(zero + 0.4_qp), 4.2364893019360180685505375326032702319e-1_qp, 61)
    call check(atanh(0.999999_qp + zero), 7.2543286192620472067417570836575386357_qp, 62)

    call check(two ** (1/two), 1.4142135623730950488016887242096979843_qp, 70)
    x = 1.7_qp + zero
    call check((zero + 3) ** x, 6.4730078399237792986648467479408190562_qp, 71)
    x = 1e5_qp + zero
    call check((1.0001_qp + zero) ** x, 2.2015456048552198645701456581654646153e4_qp, 72)
    x = zero - 30.5_qp
    call check((zero + 10) ** x, 3.1622776601683793319988935444327185951e-31_qp, 73)
    call check(hypot(zero + 3, zero + 4.5_qp), 5.4083269131959839396788319012057436171_qp, 74)
    ! (-1)**y is 1 for every even y, also for |y| > 2**16000
    x = transfer([0_int64, 9115285645797883904_int64 + n], x)   ! 2**16001
    call check((zero - 1) ** x, one, 75)
    call check((zero - 1) ** (-x), one, 76)
    call check((zero - 1) ** huge(x), one, 77)

    x = 30 + zero
    call check(sind(x), 1/two, 80)
    call check(cosd(2 * x), 1/two, 81)
    call check(tand(1.5_qp * x), one, 82)
    call check(asind(1/two), x, 83)
    call check(acosd(1/two), 2 * x, 84)
    call check(atand(one), 1.5_qp * x, 85)
    call check(atan2d(one, one), 1.5_qp * x, 86)
    ! The cosine of an odd multiple of 90 degrees is +0, not -0
    call check_positive_zero(cosd(3 * x), 87)
    call check_positive_zero(cosd(-3 * x), 88)
    call check_positive_zero(cosd(15 * x), 89)

contains

    subroutine check_positive_zero(x, code)
        real(qp), intent(in) :: x
        integer, intent(in) :: code
        if (any(transfer(x, [0_int64, 0_int64]) /= 0)) then
            print *, code, x
            error stop code
        end if
    end subroutine

    ! Within 4 epsilon: the results are within 3 ulp of the exact values.
    subroutine check(x, ref, code)
        real(qp), intent(in) :: x, ref
        integer, intent(in) :: code
        if (.not. (abs(x - ref) <= 4 * epsilon(ref) * abs(ref))) then
            print *, code, x, ref
            error stop code
        end if
    end subroutine

end program
