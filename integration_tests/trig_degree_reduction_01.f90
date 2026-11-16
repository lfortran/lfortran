program trig_degree_reduction_01
! sind, cosd and tand reduce their argument exactly in degrees, so that
! they are exact at the exact points (sind(n*360 + 30) = 0.5, ...) and
! accurate for large arguments, both at run time and in constant
! expressions (#3912).
implicit none
integer, parameter :: sp = kind(1.0), dp = kind(1.0d0)
integer, parameter :: nz4 = -huge(0) - 1, pinf4 = 2139095040, ninf4 = -8388608
integer(8), parameter :: nz8 = -huge(0_8) - 1_8, &
    pinf8 = 9218868437227405312_8, ninf8 = -4503599627370496_8
real(sp), parameter :: fs(6) = [sind(16560030.0_sp), cosd(16560060.0_sp), &
    cosd(1073746560.0_sp), tand(16560045.0_sp), sind(100000.5_sp), &
    tand(1234567.0_sp)]
real(dp), parameter :: fd(7) = [sind(360000000000030.0_dp), &
    cosd(360000000000120.0_dp), cosd(1152921504606850560.0_dp), &
    tand(360000000000135.0_dp), sind(3610.0_dp), &
    cosd(10000000017.0_dp), tand(1234567.5_dp)]
! Odd multiples of 45 degrees: +-sqrt(1/2)
real(sp), parameter :: fo4(4) = [sind(45.0_sp), cosd(45.0_sp), &
    sind(-135.0_sp), cosd(16560135.0_sp)]
real(sp) :: x, nzero4, pinf_4, ninf_4
real(dp) :: nzero8, pinf_8, ninf_8
integer :: n

nzero4 = transfer(nz4, 1.0_sp)
pinf_4 = transfer(pinf4, 1.0_sp)
ninf_4 = transfer(ninf4, 1.0_sp)
nzero8 = transfer(nz8, 1.0_dp)
pinf_8 = transfer(pinf8, 1.0_dp)
ninf_8 = transfer(ninf8, 1.0_dp)

! The program of #3912
do n = 0, 10
    x = n*360 + 30
    if (.not. same4(sind(x), 0.5_sp)) error stop 1
end do

! real(4), run time
call check4(30.0_sp, 0.5_sp, 0.0_sp, 0.0_sp, 1)
call check4(16560030.0_sp, 0.5_sp, 0.0_sp, 0.0_sp, 1)
call check4(-16560030.0_sp, -0.5_sp, 0.0_sp, 0.0_sp, 1)
call check4(16560060.0_sp, 0.0_sp, 0.5_sp, 0.0_sp, 2)
call check4(16560120.0_sp, 0.0_sp, -0.5_sp, 0.0_sp, 2)
call check4(16560045.0_sp, 0.0_sp, 0.0_sp, 1.0_sp, 4)
call check4(-16560135.0_sp, 0.0_sp, 0.0_sp, 1.0_sp, 4)
call check4(16560090.0_sp, 1.0_sp, 0.0_sp, pinf_4, 7)
call check4(16560180.0_sp, nzero4, -1.0_sp, 0.0_sp, 7)
call check4(-16560180.0_sp, 0.0_sp, -1.0_sp, nzero4, 7)
call check4(16560270.0_sp, -1.0_sp, 0.0_sp, ninf_4, 7)
call check4(16560360.0_sp, 0.0_sp, 1.0_sp, 0.0_sp, 7)
call check4(1073744640.0_sp, 0.0_sp, 1.0_sp, 0.0_sp, 7)
call check4(1073746560.0_sp, 0.0_sp, -0.5_sp, 0.0_sp, 2)
call check4(1073742720.0_sp, 0.0_sp, -0.5_sp, 0.0_sp, 2)
call near4(sind(id4(100000.5_sp)), -0.9832549095153809_sp)
call near4(cosd(id4(16000001.0_sp)), -0.9455185532569885_sp)
call near4(tand(id4(1234567.0_sp)), -1.3270448446273804_sp)

! real(8), run time
call check8(30.0_dp, 0.5_dp, 0.0_dp, 0.0_dp, 1)
call check8(360000000000030.0_dp, 0.5_dp, 0.0_dp, 0.0_dp, 1)
call check8(-360000000000030.0_dp, -0.5_dp, 0.0_dp, 0.0_dp, 1)
call check8(60.0_dp, 0.0_dp, 0.5_dp, 0.0_dp, 2)
call check8(120.0_dp, 0.0_dp, -0.5_dp, 0.0_dp, 2)
call check8(360000000000060.0_dp, 0.0_dp, 0.5_dp, 0.0_dp, 2)
call check8(135.0_dp, 0.0_dp, 0.0_dp, -1.0_dp, 4)
call check8(360000000000045.0_dp, 0.0_dp, 0.0_dp, 1.0_dp, 4)
call check8(90.0_dp, 1.0_dp, 0.0_dp, pinf_8, 7)
call check8(180.0_dp, nzero8, -1.0_dp, 0.0_dp, 7)
call check8(-180.0_dp, 0.0_dp, -1.0_dp, nzero8, 7)
call check8(270.0_dp, -1.0_dp, 0.0_dp, ninf_8, 7)
call check8(360000000000090.0_dp, 1.0_dp, 0.0_dp, pinf_8, 7)
call check8(360000000000360.0_dp, 0.0_dp, 1.0_dp, 0.0_dp, 7)
call check8(1152921504606854400.0_dp, 0.0_dp, 1.0_dp, 0.0_dp, 7)
call check8(1152921504606850560.0_dp, 0.0_dp, -0.5_dp, 0.0_dp, 2)
call check8(1152921504606858240.0_dp, 0.0_dp, -0.5_dp, 0.0_dp, 2)
call check8(1267650600228233342146377154560.0_dp, 0.0_dp, -0.5_dp, 0.0_dp, 2)
call near8(sind(id8(3610.0_dp)), 0.17364817766693036_dp)
call near8(cosd(id8(10000000017.0_dp)), 0.4539904997395468_dp)
call near8(tand(id8(1234567.5_dp)), -1.3032253728412058_dp)
call near8(sind(id8(1152921504606847232.0_dp)), 0.5299192642332049_dp)
call near8(cosd(id8(123456789.25_dp)), -0.9869963665602319_dp)

! constant expressions
if (.not. same4(fs(1), 0.5_sp)) error stop 21
if (.not. same4(fs(2), 0.5_sp)) error stop 22
if (.not. same4(fs(3), -0.5_sp)) error stop 23
if (.not. same4(fs(4), 1.0_sp)) error stop 24
call near4(fs(5), -0.9832549095153809_sp)
call near4(fs(6), -1.3270448446273804_sp)
if (.not. same8(fd(1), 0.5_dp)) error stop 31
if (.not. same8(fd(2), -0.5_dp)) error stop 32
if (.not. same8(fd(3), -0.5_dp)) error stop 33
if (.not. same8(fd(4), -1.0_dp)) error stop 34
call near8(fd(5), 0.17364817766693036_dp)
call near8(fd(6), 0.4539904997395468_dp)
call near8(fd(7), -1.3032253728412058_dp)

! Odd multiples of 45 degrees are sqrt(1/2) correctly rounded, with the
! sign of the octant
call octant4(45.0_sp, 1.0_sp, 1.0_sp)
call octant4(135.0_sp, 1.0_sp, -1.0_sp)
call octant4(225.0_sp, -1.0_sp, -1.0_sp)
call octant4(-315.0_sp, 1.0_sp, 1.0_sp)
call octant4(16560045.0_sp, 1.0_sp, 1.0_sp)
call octant4(-16560135.0_sp, -1.0_sp, -1.0_sp)
if (.not. same4(fo4(1), sqrt(0.5_sp))) error stop 41
if (.not. same4(fo4(2), sqrt(0.5_sp))) error stop 42
if (.not. same4(fo4(3), -sqrt(0.5_sp))) error stop 43
if (.not. same4(fo4(4), -sqrt(0.5_sp))) error stop 44
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

    real(sp) function id4(a)
        real(sp), intent(in) :: a
        id4 = a
    end function

    real(dp) function id8(a)
        real(dp), intent(in) :: a
        id8 = a
    end function

    ! Checks the exact values selected by `which`: 1 sind, 2 cosd,
    ! 4 tand (bits may be combined).
    subroutine check4(a, s, c, t, which)
        real(sp), intent(in) :: a, s, c, t
        integer, intent(in) :: which
        if (iand(which, 1) /= 0) then
            if (.not. same4(sind(a), s)) error stop 2
        end if
        if (iand(which, 2) /= 0) then
            if (.not. same4(cosd(a), c)) error stop 3
        end if
        if (iand(which, 4) /= 0) then
            if (.not. same4(tand(a), t)) error stop 4
        end if
    end subroutine

    subroutine check8(a, s, c, t, which)
        real(dp), intent(in) :: a, s, c, t
        integer, intent(in) :: which
        if (iand(which, 1) /= 0) then
            if (.not. same8(sind(a), s)) error stop 5
        end if
        if (iand(which, 2) /= 0) then
            if (.not. same8(cosd(a), c)) error stop 6
        end if
        if (iand(which, 4) /= 0) then
            if (.not. same8(tand(a), t)) error stop 7
        end if
    end subroutine

    ! sind(a) and cosd(a) are sg*sqrt(1/2) and cg*sqrt(1/2)
    subroutine octant4(a, sg, cg)
        real(sp), intent(in) :: a, sg, cg
        real(sp) :: h
        h = sqrt(0.5_sp)
        if (.not. same4(sind(a), sg*h)) error stop 10
        if (.not. same4(cosd(a), cg*h)) error stop 11
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
