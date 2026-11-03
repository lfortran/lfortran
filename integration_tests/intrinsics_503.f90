program intrinsics_503
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64
    implicit none
    integer(int8), parameter :: s8 = ibset(0_int8, 7)
    integer(int16), parameter :: s16 = ibset(0_int16, 15)
    integer(int32), parameter :: s32 = ibset(0_int32, 31)
    integer(int64), parameter :: s64 = ibset(0_int64, 63)
    integer(int8), parameter :: min8 = -huge(0_int8) - 1_int8
    integer(int16), parameter :: min16 = -huge(0_int16) - 1_int16
    integer(int32), parameter :: min32 = -huge(0_int32) - 1_int32
    integer(int64), parameter :: min64 = -huge(0_int64) - 1_int64

    if (s8 >= 0_int8) error stop 1
    if (s16 >= 0_int16) error stop 2
    if (s32 >= 0_int32) error stop 3
    if (s64 >= 0_int64) error stop 4
    if (int(s8, int64) /= int(min8, int64)) error stop 5
    if (int(s16, int64) /= int(min16, int64)) error stop 6
    if (int(s32, int64) /= int(min32, int64)) error stop 7
    if (s64 /= min64) error stop 8
    if (ibset(1_int8, 7) /= min8 + 1_int8) error stop 9
    if (ibset(1_int16, 15) /= min16 + 1_int16) error stop 10
    if (ibset(1_int32, 31) /= min32 + 1_int32) error stop 11
    if (ibset(1_int64, 63) /= min64 + 1_int64) error stop 12

    call check_runtime(0_int8, 0_int16, 0_int32, 0_int64)

contains

    subroutine check_runtime(z8, z16, z32, z64)
        integer(int8), intent(in) :: z8
        integer(int16), intent(in) :: z16
        integer(int32), intent(in) :: z32
        integer(int64), intent(in) :: z64

        if (ibset(z8, 7) /= s8) error stop 13
        if (ibset(z16, 15) /= s16) error stop 14
        if (ibset(z32, 31) /= s32) error stop 15
        if (ibset(z64, 63) /= s64) error stop 16
        if (ibset(z8, 6) /= ibset(0_int8, 6)) error stop 17
        if (ibset(z16, 14) /= ibset(0_int16, 14)) error stop 18
        if (ibset(z32, 30) /= ibset(0_int32, 30)) error stop 19
        if (ibset(z64, 62) /= ibset(0_int64, 62)) error stop 20
        if (ibset(z8 + 1_int8, 7) /= ibset(1_int8, 7)) error stop 21
        if (ibset(z16 + 1_int16, 15) /= ibset(1_int16, 15)) error stop 22
        if (ibset(z32 + 1_int32, 31) /= ibset(1_int32, 31)) error stop 23
        if (ibset(z64 + 1_int64, 63) /= ibset(1_int64, 63)) error stop 24
        if (ibset(z8 - 1_int8, 7) /= ibset(-1_int8, 7)) error stop 25
        if (ibset(z16 - 1_int16, 15) /= ibset(-1_int16, 15)) error stop 26
        if (ibset(z32 - 1_int32, 31) /= ibset(-1_int32, 31)) error stop 27
        if (ibset(z64 - 1_int64, 63) /= ibset(-1_int64, 63)) error stop 28
    end subroutine

end program
