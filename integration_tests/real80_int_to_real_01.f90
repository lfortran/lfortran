program real80_int_to_real_01
    ! Integer constants that initialize a real(10) parameter are folded
    ! exactly, in both the `::` form and the PARAMETER statement (#15651).
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    real(10), parameter :: big = 9007199254740993_int64
    real(10) :: s
    parameter (s = 9007199254740993_int64)
    real(10) :: t
    parameter (t = -9007199254740993_int64)

    if (big - 9007199254740992.0_10 /= 1) error stop 1
    if (s - 9007199254740992.0_10 /= 1) error stop 2
    if (t + 9007199254740992.0_10 /= -1) error stop 3
    if (s /= big) error stop 4
end program real80_int_to_real_01
