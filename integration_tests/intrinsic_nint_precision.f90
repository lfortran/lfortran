program intrinsic_nint_precision
    implicit none
    integer, parameter :: folded4 = nint(8388609.0)
    integer(8), parameter :: folded8 = nint(4503599627370497.0_8, kind=8)
    integer(8), parameter :: folded_min = nint(-9223372036854774784.0_8, kind=8)
    integer(8), parameter :: folded_max = nint(9223372036854774784.0_8, kind=8)

    if (folded4 /= 8388609) error stop 1
    if (folded8 /= 4503599627370497_8) error stop 2
    if (folded_min /= -9223372036854774784_8) error stop 3
    if (folded_max /= 9223372036854774784_8) error stop 4
    call check4(8388609.0, folded4)
    call check4(-8388609.0, -folded4)
    call check4(16777215.0, 16777215)
    call check4(-16777215.0, -16777215)
    call check4(2147483520.0, 2147483520)
    call check4(-2147483520.0, -2147483520)
    call check4(8388607.5, 8388608)
    call check4(-8388607.5, -8388608)
    call check4(0.4999999701976776123046875, 0)
    call check4(-0.4999999701976776123046875, 0)
    call check4(0.5, 1)
    call check4(-0.5, -1)
    call check4(0.500000059604644775390625, 1)
    call check4(-0.500000059604644775390625, -1)
    call check4(1.49999988079071044921875, 1)
    call check4(-1.49999988079071044921875, -1)
    call check4(1.5, 2)
    call check4(-1.5, -2)
    call check4(1.50000011920928955078125, 2)
    call check4(-1.50000011920928955078125, -2)
    call check8(4503599627370497.0_8, folded8)
    call check8(-4503599627370497.0_8, -folded8)
    call check8(9007199254740991.0_8, 9007199254740991_8)
    call check8(-9007199254740991.0_8, -9007199254740991_8)
    call check8(4503599627370495.5_8, 4503599627370496_8)
    call check8(-4503599627370495.5_8, -4503599627370496_8)
    call check8(0.499999999999999944488848768742172978818416595458984375_8, 0_8)
    call check8(-0.499999999999999944488848768742172978818416595458984375_8, 0_8)
    call check8(0.5_8, 1_8)
    call check8(-0.5_8, -1_8)
    call check8(0.50000000000000011102230246251565404236316680908203125_8, 1_8)
    call check8(-0.50000000000000011102230246251565404236316680908203125_8, -1_8)
    call check8(1.4999999999999997779553950749686919152736663818359375_8, 1_8)
    call check8(-1.4999999999999997779553950749686919152736663818359375_8, -1_8)
    call check8(1.5_8, 2_8)
    call check8(-1.5_8, -2_8)
    call check8(1.5000000000000002220446049250313080847263336181640625_8, 2_8)
    call check8(-1.5000000000000002220446049250313080847263336181640625_8, -2_8)
    call check8(-9223372036854774784.0_8, folded_min)
    call check8(9223372036854774784.0_8, folded_max)
    call check_narrow(2147483647.0_8, huge(0))
    call check_narrow(-2147483647.0_8, -huge(0))
    call check_narrow(2147483646.5_8, huge(0))
    call check_narrow(-2147483646.5_8, -huge(0))
contains
    subroutine check4(x, expected)
        real, intent(in) :: x
        integer, intent(in) :: expected
        if (nint(x) /= expected) error stop 5
        if (nint(x, kind=8) /= int(expected, 8)) error stop 6
    end subroutine

    subroutine check8(x, expected)
        real(8), intent(in) :: x
        integer(8), intent(in) :: expected
        if (nint(x, kind=8) /= expected) error stop 9
    end subroutine

    subroutine check_narrow(x, expected)
        real(8), intent(in) :: x
        integer, intent(in) :: expected
        if (nint(x) /= expected) error stop 12
    end subroutine
end program
