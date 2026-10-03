program intrinsics_475
    ! Verify ishftc with the optional SIZE argument preserves the bits of I
    ! that lie above position SIZE. Only the rightmost SIZE bits are rotated;
    ! the higher bits must be left unchanged. (See F2018 16.9.130.)

    implicit none

    integer(kind=1) :: res_1
    integer(kind=2) :: res_2
    integer(kind=4) :: res_4
    integer(kind=8) :: res_8

    integer(kind=4), parameter :: p1 = ishftc(120_4, 2_4, 4_4)
    integer(kind=8), parameter :: p2 = ishftc(120_8, -2_8, 4_8)

    ! ----- compile-time fold (parameter) path -----

    ! 120 = 0b01111000: low 4 bits 1000 rotate left 2 -> 0010, high 01110000 kept
    if (p1 /= 114) error stop
    ! same value rotated right by 2: low 4 bits 1000 rotate right 2 -> 0010
    if (p2 /= 114_8) error stop

    if (ishftc(7_4, 2_4, 3_4)   /= 7)   error stop   ! 111 rot L2 -> 111
    if (ishftc(8_4, 1_4, 4_4)   /= 1)   error stop   ! 1000 rot L1 -> 0001
    if (ishftc(1_4, 1_4, 4_4)   /= 2)   error stop   ! 0001 rot L1 -> 0010
    if (ishftc(255_4, 4_4, 4_4) /= 255) error stop  ! low nibble 1111 unchanged
    if (ishftc(240_4, 1_4, 4_4) /= 240) error stop  ! low nibble 0000 unchanged
    if (ishftc(176_4, 1_4, 4_4) /= 176) error stop  ! low nibble 0000 unchanged
    if (ishftc(49_4, 2_4, 4_4)  /= 52)  error stop  ! low 0001 rot L2 -> 0100
    if (ishftc(49_4, -2_4, 4_4) /= 52)  error stop  ! low 0001 rot R2 -> 0100

    ! ----- runtime path (values not known at compile time) -----

    res_4 = runtime_i4(120_4, 2_4, 4_4)
    if (res_4 /= 114) error stop
    res_4 = runtime_i4(120_4, -2_4, 4_4)
    if (res_4 /= 114) error stop
    res_8 = runtime_i8(120_8, 2_8, 4_8)
    if (res_8 /= 114_8) error stop

    ! ----- all kinds, identical rotation (high bits preserved) -----

    res_1 = ishftc(120_1, 2_1, 4_1)
    if (res_1 /= 114) error stop
    res_2 = ishftc(120_2, 2_2, 4_2)
    if (res_2 /= 114) error stop
    res_4 = ishftc(120_4, 2_4, 4_4)
    if (res_4 /= 114) error stop
    res_8 = ishftc(120_8, 2_8, 4_8)
    if (res_8 /= 114_8) error stop

    ! ----- negative shift keeps high bits too -----

    res_1 = ishftc(120_1, -2_1, 4_1)
    if (res_1 /= 114) error stop
    res_2 = ishftc(120_2, -2_2, 4_2)
    if (res_2 /= 114) error stop
    res_4 = ishftc(120_4, -2_4, 4_4)
    if (res_4 /= 114) error stop
    res_8 = ishftc(120_8, -2_8, 4_8)
    if (res_8 /= 114_8) error stop

    ! ----- SHIFT == 0: value is unchanged (early-return path) -----

    if (ishftc(120_4, 0_4, 4_4)   /= 120)           error stop
    if (ishftc(120_4, 0_4, 32_4)  /= 120)           error stop
    if (ishftc(120_8, 0_8, 64_8)  /= 120_8)         error stop
    if (ishftc(huge(0_8), 0_8, 64_8) /= huge(0_8))  error stop

    ! ----- SIZE == BIT_SIZE(I): full-width rotation (mask == ~0ULL branch) -----

    ! kind=4, SIZE=32
    if (ishftc(120_4, 2_4, 32_4)  /= 480)           error stop
    ! kind=8, SIZE=64 -> exercises the bits_size == 64 mask branch
    if (ishftc(120_8, 2_8, 64_8)  /= 480_8)         error stop
    if (ishftc(120_8, -2_8, 64_8) /= 30_8)          error stop
    if (ishftc(huge(0_8), 2_8, 64_8) /= -3_8)       error stop

    print *, "pass"

contains

    function runtime_i4(i, s, z) result(r)
        integer(kind=4), intent(in) :: i, s, z
        integer(kind=4) :: r
        r = ishftc(i, s, z)
    end function runtime_i4

    function runtime_i8(i, s, z) result(r)
        integer(kind=8), intent(in) :: i, s, z
        integer(kind=8) :: r
        r = ishftc(i, s, z)
    end function runtime_i8

end program
