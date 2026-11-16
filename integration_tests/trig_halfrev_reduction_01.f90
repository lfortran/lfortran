program trig_halfrev_reduction_01
! sinpi, cospi and tanpi reduce their argument exactly in half revolutions:
! they are exact at the exact points, with the IEEE 754 signs of zero and
! infinity, and accurate for large arguments, both at run time and in
! constant expressions (#3912).
implicit none
integer, parameter :: sp = kind(1.0), dp = kind(1.0d0), qp = 16
integer, parameter :: nz4 = -huge(0) - 1, pinf4 = 2139095040, ninf4 = -8388608
integer(8), parameter :: nz8 = -huge(0_8) - 1_8, &
    pinf8 = 9218868437227405312_8, ninf8 = -4503599627370496_8
real(sp), parameter :: fs(6) = [sinpi(1.0_sp), cospi(1000000.5_sp), &
    tanpi(1000000.25_sp), sinpi(1000.1_sp), cospi(3.3_sp), tanpi(0.4999_sp)]
real(dp), parameter :: fd(7) = [sinpi(1.0_dp), cospi(4503599627370497.0_dp), &
    tanpi(1000000.75_dp), sinpi(1000000.5_dp), sinpi(0.1_dp), &
    cospi(3.3_dp), tanpi(1000000.2_dp)]
! Half-integers in [2^51, 2^52): 2*|x| is an odd integer
real(dp), parameter :: fh(12) = [sinpi(2251799813685248.5_dp), &
    cospi(2251799813685248.5_dp), tanpi(2251799813685248.5_dp), &
    sinpi(2251799813685249.5_dp), cospi(2251799813685249.5_dp), &
    tanpi(2251799813685249.5_dp), sinpi(-2251799813685248.5_dp), &
    cospi(-2251799813685248.5_dp), tanpi(-2251799813685248.5_dp), &
    sinpi(4503599627370495.5_dp), cospi(4503599627370495.5_dp), &
    tanpi(4503599627370495.5_dp)]
! Just below a quarter turn: 2*|x| + 1/2 is not representable
real(sp), parameter :: q4 = nearest(0.25_sp, -1.0_sp)
real(dp), parameter :: q8 = nearest(0.25_dp, -1.0_dp)
real(qp), parameter :: q16 = nearest(0.25_qp, -1.0_qp)
real(sp), parameter :: fq4(6) = [sinpi(q4), cospi(q4), tanpi(q4), &
    sinpi(-q4), cospi(-q4), tanpi(-q4)]
real(dp), parameter :: fq8(6) = [sinpi(q8), cospi(q8), tanpi(q8), &
    sinpi(-q8), cospi(-q8), tanpi(-q8)]
real(qp), parameter :: fq16(6) = [sinpi(q16), cospi(q16), tanpi(q16), &
    sinpi(-q16), cospi(-q16), tanpi(-q16)]
! Odd multiples of 45 degrees and of a quarter turn: +-sqrt(1/2)
real(dp), parameter :: fo8(8) = [sind(45.0_dp), cosd(45.0_dp), &
    sind(-135.0_dp), cosd(-135.0_dp), sinpi(0.25_dp), cospi(0.25_dp), &
    sinpi(-0.75_dp), cospi(-0.75_dp)]
real(sp) :: nzero4, pinf_4, ninf_4
real(dp) :: nzero8, pinf_8, ninf_8
real(qp) :: nzero16, pinf_16, ninf_16

nzero4 = transfer(nz4, 1.0_sp)
pinf_4 = transfer(pinf4, 1.0_sp)
ninf_4 = transfer(ninf4, 1.0_sp)
nzero8 = transfer(nz8, 1.0_dp)
pinf_8 = transfer(pinf8, 1.0_dp)
ninf_8 = transfer(ninf8, 1.0_dp)
nzero16 = real(nzero8, qp)
pinf_16 = real(pinf_8, qp)
ninf_16 = real(ninf_8, qp)

! real(4), run time; `which` selects 1 sinpi, 2 cospi, 4 tanpi
call check4(1.0_sp, 0.0_sp, -1.0_sp, nzero4, 7)
call check4(-1.0_sp, nzero4, -1.0_sp, 0.0_sp, 7)
call check4(2.0_sp, 0.0_sp, 1.0_sp, 0.0_sp, 7)
call check4(nzero4, nzero4, 1.0_sp, nzero4, 7)
call check4(0.5_sp, 1.0_sp, 0.0_sp, pinf_4, 7)
call check4(1.5_sp, -1.0_sp, 0.0_sp, ninf_4, 7)
call check4(-0.5_sp, -1.0_sp, 0.0_sp, ninf_4, 7)
call check4(0.25_sp, 0.0_sp, 0.0_sp, 1.0_sp, 4)
call check4(-0.75_sp, 0.0_sp, 0.0_sp, 1.0_sp, 4)
call check4(1000000.5_sp, 1.0_sp, 0.0_sp, pinf_4, 7)
call check4(1000001.0_sp, 0.0_sp, -1.0_sp, nzero4, 7)
call check4(1000000.75_sp, 0.0_sp, 0.0_sp, -1.0_sp, 4)
call check4(16777217.0_sp, 0.0_sp, 1.0_sp, 0.0_sp, 7)
call near4(sinpi(id4(1000.1_sp)), 0.30894404649734497_sp)
call near4(cospi(id4(3.3_sp)), -0.5877853631973267_sp)
call near4(tanpi(id4(0.4999_sp)), 3183.519287109375_sp)

! real(8), run time
call check8(1.0_dp, 0.0_dp, -1.0_dp, nzero8, 7)
call check8(-1.0_dp, nzero8, -1.0_dp, 0.0_dp, 7)
call check8(nzero8, nzero8, 1.0_dp, nzero8, 7)
call check8(0.5_dp, 1.0_dp, 0.0_dp, pinf_8, 7)
call check8(1.5_dp, -1.0_dp, 0.0_dp, ninf_8, 7)
call check8(0.25_dp, 0.0_dp, 0.0_dp, 1.0_dp, 4)
call check8(1000000.5_dp, 1.0_dp, 0.0_dp, pinf_8, 7)
call check8(1000000.75_dp, 0.0_dp, 0.0_dp, -1.0_dp, 4)
call check8(4503599627370497.0_dp, 0.0_dp, -1.0_dp, nzero8, 7)
call check8(2251799813685248.5_dp, 1.0_dp, 0.0_dp, pinf_8, 7)
call check8(2251799813685249.5_dp, -1.0_dp, 0.0_dp, ninf_8, 7)
call check8(-2251799813685248.5_dp, -1.0_dp, 0.0_dp, ninf_8, 7)
call check8(4503599627370495.5_dp, -1.0_dp, 0.0_dp, ninf_8, 7)
call check8(9007199254740994.0_dp, 0.0_dp, 1.0_dp, 0.0_dp, 7)
call check8(1.0e300_dp, 0.0_dp, 1.0_dp, 0.0_dp, 7)
call near8(sinpi(id8(1000000.3333333334_dp)), 0.8660254038453936_dp)
call near8(tanpi(id8(1000000.2_dp)), 0.7265425277818469_dp)
call near8(sinpi(id8(0.1_dp)), 0.30901699437494745_dp)
call near8(cospi(id8(3.3_dp)), -0.5877852522924736_dp)
call near8(tanpi(id8(0.4999_dp)), 3183.0987571185015_dp)

! real(16), run time
call check16(1.0_qp, 0.0_qp, -1.0_qp, nzero16, 7)
call check16(-1.0_qp, nzero16, -1.0_qp, 0.0_qp, 7)
call check16(0.5_qp, 1.0_qp, 0.0_qp, pinf_16, 7)
call check16(1.5_qp, -1.0_qp, 0.0_qp, ninf_16, 7)
call check16(0.25_qp, 0.0_qp, 0.0_qp, 1.0_qp, 4)
call check16(1000000.5_qp, 1.0_qp, 0.0_qp, pinf_16, 7)
call check16(1000000.25_qp, 0.0_qp, 0.0_qp, 1.0_qp, 4)

! constant expressions
if (.not. same4(fs(1), 0.0_sp)) error stop 21
if (.not. same4(fs(2), 0.0_sp)) error stop 22
if (.not. same4(fs(3), 1.0_sp)) error stop 23
call near4(fs(4), 0.30894404649734497_sp)
call near4(fs(5), -0.5877853631973267_sp)
call near4(fs(6), 3183.519287109375_sp)
if (.not. same8(fd(1), 0.0_dp)) error stop 31
if (.not. same8(fd(2), -1.0_dp)) error stop 32
if (.not. same8(fd(3), -1.0_dp)) error stop 33
if (.not. same8(fd(4), 1.0_dp)) error stop 34
call near8(fd(5), 0.30901699437494745_dp)
call near8(fd(6), -0.5877852522924736_dp)
call near8(fd(7), 0.7265425277818469_dp)
if (.not. same8(fh(1), 1.0_dp)) error stop 41
if (.not. same8(fh(2), 0.0_dp)) error stop 42
if (.not. same8(fh(3), pinf_8)) error stop 43
if (.not. same8(fh(4), -1.0_dp)) error stop 44
if (.not. same8(fh(5), 0.0_dp)) error stop 45
if (.not. same8(fh(6), ninf_8)) error stop 46
if (.not. same8(fh(7), -1.0_dp)) error stop 47
if (.not. same8(fh(8), 0.0_dp)) error stop 48
if (.not. same8(fh(9), ninf_8)) error stop 49
if (.not. same8(fh(10), -1.0_dp)) error stop 50
if (.not. same8(fh(11), 0.0_dp)) error stop 51
if (.not. same8(fh(12), ninf_8)) error stop 52

! Just below a quarter turn, run time and constant expressions
call quarter4(sinpi(id4(q4)), cospi(id4(q4)), tanpi(id4(q4)), 1.0_sp)
call quarter4(sinpi(id4(-q4)), cospi(id4(-q4)), tanpi(id4(-q4)), -1.0_sp)
call quarter4(fq4(1), fq4(2), fq4(3), 1.0_sp)
call quarter4(fq4(4), fq4(5), fq4(6), -1.0_sp)
call quarter8(sinpi(id8(q8)), cospi(id8(q8)), tanpi(id8(q8)), 1.0_dp)
call quarter8(sinpi(id8(-q8)), cospi(id8(-q8)), tanpi(id8(-q8)), -1.0_dp)
call quarter8(fq8(1), fq8(2), fq8(3), 1.0_dp)
call quarter8(fq8(4), fq8(5), fq8(6), -1.0_dp)
call quarter16(sinpi(id16(q16)), cospi(id16(q16)), tanpi(id16(q16)), 1.0_qp)
call quarter16(sinpi(id16(-q16)), cospi(id16(-q16)), tanpi(id16(-q16)), &
    -1.0_qp)
call quarter16(fq16(1), fq16(2), fq16(3), 1.0_qp)
call quarter16(fq16(4), fq16(5), fq16(6), -1.0_qp)

! Odd multiples of 45 degrees and of a quarter turn are sqrt(1/2) correctly
! rounded, with the sign of the octant, in degrees and in half turns alike
! (GFortran's sind(45.0_dp) is one ulp low, so these real(8) degree checks
! are here rather than in trig_degree_reduction_01).
call octant8(45.0_dp, 1.0_dp, 1.0_dp)
call octant8(135.0_dp, 1.0_dp, -1.0_dp)
call octant8(225.0_dp, -1.0_dp, -1.0_dp)
call octant8(315.0_dp, -1.0_dp, 1.0_dp)
call octant8(-45.0_dp, -1.0_dp, 1.0_dp)
call octant8(-225.0_dp, 1.0_dp, -1.0_dp)
call octant8(360000000000045.0_dp, 1.0_dp, 1.0_dp)
call octant8(180000000000225.0_dp, -1.0_dp, -1.0_dp)
call octant8(1000000.25_dp*180, 1.0_dp, 1.0_dp)
call octant8(-1000000.75_dp*180, -1.0_dp, -1.0_dp)
if (.not. same8(fo8(1), sqrt(0.5_dp))) error stop 61
if (.not. same8(fo8(2), sqrt(0.5_dp))) error stop 62
if (.not. same8(fo8(3), -sqrt(0.5_dp))) error stop 63
if (.not. same8(fo8(4), -sqrt(0.5_dp))) error stop 64
if (.not. same8(fo8(5), sqrt(0.5_dp))) error stop 65
if (.not. same8(fo8(6), sqrt(0.5_dp))) error stop 66
if (.not. same8(fo8(7), -sqrt(0.5_dp))) error stop 67
if (.not. same8(fo8(8), -sqrt(0.5_dp))) error stop 68
print *, "ok"

contains

    logical function same4(a, b)
        real(sp), intent(in) :: a, b
        same4 = transfer(a, 0) == transfer(b, 0)
    end function

    logical function same8(a, b)
        real(dp), intent(in) :: a, b
        same8 = transfer(a, 0_8) == transfer(b, 0_8)
    end function

    logical function same16(a, b)
        real(qp), intent(in) :: a, b
        integer(8) :: ia(2), ib(2)
        ia = transfer(a, ia)
        ib = transfer(b, ib)
        same16 = ia(1) == ib(1) .and. ia(2) == ib(2)
    end function

    real(sp) function id4(a)
        real(sp), intent(in) :: a
        id4 = a
    end function

    real(dp) function id8(a)
        real(dp), intent(in) :: a
        id8 = a
    end function

    real(qp) function id16(a)
        real(qp), intent(in) :: a
        id16 = a
    end function

    subroutine check4(a, s, c, t, which)
        real(sp), intent(in) :: a, s, c, t
        integer, intent(in) :: which
        if (iand(which, 1) /= 0) then
            if (.not. same4(sinpi(a), s)) error stop 2
        end if
        if (iand(which, 2) /= 0) then
            if (.not. same4(cospi(a), c)) error stop 3
        end if
        if (iand(which, 4) /= 0) then
            if (.not. same4(tanpi(a), t)) error stop 4
        end if
    end subroutine

    subroutine check8(a, s, c, t, which)
        real(dp), intent(in) :: a, s, c, t
        integer, intent(in) :: which
        if (iand(which, 1) /= 0) then
            if (.not. same8(sinpi(a), s)) error stop 5
        end if
        if (iand(which, 2) /= 0) then
            if (.not. same8(cospi(a), c)) error stop 6
        end if
        if (iand(which, 4) /= 0) then
            if (.not. same8(tanpi(a), t)) error stop 7
        end if
    end subroutine

    subroutine check16(a, s, c, t, which)
        real(qp), intent(in) :: a, s, c, t
        integer, intent(in) :: which
        if (iand(which, 1) /= 0) then
            if (.not. same16(sinpi(a), s)) error stop 10
        end if
        if (iand(which, 2) /= 0) then
            if (.not. same16(cospi(a), c)) error stop 11
        end if
        if (iand(which, 4) /= 0) then
            if (.not. same16(tanpi(a), t)) error stop 12
        end if
    end subroutine

    ! sind and cosd of a degrees, and sinpi and cospi of a/180 half turns,
    ! are sg*sqrt(1/2) and cg*sqrt(1/2).
    subroutine octant8(a, sg, cg)
        real(dp), intent(in) :: a, sg, cg
        real(dp) :: h
        h = sqrt(0.5_dp)
        if (.not. same8(sind(a), sg*h)) error stop 18
        if (.not. same8(cosd(a), cg*h)) error stop 18
        if (.not. same8(sinpi(a/180), sg*h)) error stop 19
        if (.not. same8(cospi(a/180), cg*h)) error stop 19
    end subroutine

    ! sinpi, cospi and tanpi of sg*nearest(0.25, -1.0): sin(pi*x) < cos(pi*x)
    ! and tan(pi*x) < 1 by about 2 ulp, so neither is rounded to the value
    ! at a quarter turn (#3912).
    subroutine quarter4(s, c, t, sg)
        real(sp), intent(in) :: s, c, t, sg
        call near4(s, sg*0.7071067690849304_sp)
        call near4(c, 0.7071068286895752_sp)
        call near4(t, sg*0.9999998807907104_sp)
        if (.not. abs(s) <= c .or. .not. abs(t) < 1) error stop 13
    end subroutine

    subroutine quarter8(s, c, t, sg)
        real(dp), intent(in) :: s, c, t, sg
        call near8(s, sg*0.7071067811865475_dp)
        call near8(c, 0.7071067811865476_dp)
        call near8(t, sg*0.9999999999999998_dp)
        if (.not. abs(s) <= c .or. .not. abs(t) < 1) error stop 14
    end subroutine

    ! binary128 is correctly rounded here: sinpi is sqrt(1/2), cospi is
    ! one ulp above it, and tanpi is two ulp below 1.
    subroutine quarter16(s, c, t, sg)
        real(qp), intent(in) :: s, c, t, sg
        real(qp) :: h
        h = sqrt(0.5_qp)
        if (.not. same16(s, sg*h)) error stop 15
        if (.not. same16(c, nearest(h, 1.0_qp))) error stop 16
        if (.not. same16(t, sg*nearest(nearest(1.0_qp, -1.0_qp), -1.0_qp))) &
            error stop 17
    end subroutine

    ! Within one ulp of the correctly rounded reference value
    subroutine near4(y, ref)
        real(sp), intent(in) :: y, ref
        if (abs(y - ref) > spacing(ref)) error stop 8
    end subroutine

    subroutine near8(y, ref)
        real(dp), intent(in) :: y, ref
        if (abs(y - ref) > spacing(ref)) error stop 9
    end subroutine

end program
