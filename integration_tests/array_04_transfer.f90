module array_04_transfer_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64, real32
    implicit none
    integer(int32), parameter :: sc_constsub = int(z'deadbeef', int32)
    integer(int32), parameter :: int32_arr(2) = [sc_constsub, sc_constsub]
    real(real32),  parameter :: real32_arr(2) = [real(1.23, real32), real(4.56, real32)]
    integer(int64), parameter :: int32_int64 = transfer(int32_arr, 0_int64)
    integer(int64), parameter :: real32_int64 = transfer(real32_arr, 0_int64)
    integer(int64), parameter :: real32_int32 = transfer(real32_arr, 0_int32)
end module
program array_04_transfer
    use array_04_transfer_mod
    implicit none
    real :: value(5) = [1.1, 1.2, 1.3, 1.4, 1.5]
    integer :: val(5)
    val = transfer(value, val, 1 * size(value))
    print * , val
    if (all(val /= [1066192077, 1067030938, 1067869798, 1068708659, 1069547520])) error stop
    if (real32_int32 /= 1067282596) error stop
    if (real32_int64 /= 4652758847580893348_8) error stop
    if (int32_int64 /= -2401053088876216593_8) error stop
end program