program test_ieee_nan_value
    use ieee_arithmetic
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none

    real(real64) :: dp_snan, dp_qnan
    real(real32) :: sp_qnan
    integer(int64) :: ibits64
    integer(int32) :: ibits32
    real(real64) :: dpnan(2)
    integer(int32) :: transfer_result(2)

    ! Double-precision signaling NaN bit pattern (matches gfortran)
    dp_snan = ieee_value(1.0_real64, ieee_signaling_nan)
    transfer_result = transfer(dp_snan, [1])
    if (transfer_result(1) /= 0) &
        error stop "dp snan low word mismatch"
    if (transfer_result(2) /= 2146697216) &
        error stop "dp snan high word mismatch"

    ! Double-precision quiet NaN bit pattern
    dp_qnan = ieee_value(1.0_real64, ieee_quiet_nan)
    ibits64 = transfer(dp_qnan, 0_int64)
    if (ibits64 /= int(z'7FF8000000000000', int64)) &
        error stop "dp quiet NaN bit pattern mismatch"

    ! Single-precision quiet NaN bit pattern
    sp_qnan = ieee_value(1.0_real32, ieee_quiet_nan)
    ibits32 = transfer(sp_qnan, 0_int32)
    if (ibits32 /= int(z'7FC00000', int32)) &
        error stop "sp quiet NaN bit pattern mismatch"

    ! Transfer double to int32 array (tests byte reinterpretation)
    dp_qnan = ieee_value(1.0_real64, ieee_quiet_nan)
    transfer_result = transfer(dp_qnan, [1])
    if (transfer_result(1) /= 0) &
        error stop "transfer qnan low word mismatch"
    if (transfer_result(2) /= 2146959360) &
        error stop "transfer qnan high word mismatch"

    ! Transfer known double (1.0d0) to int32 array
    transfer_result = transfer(1.0_real64, [1])
    if (transfer_result(1) /= 0) &
        error stop "transfer 1.0d0 low word mismatch"
    if (transfer_result(2) /= 1072693248) &
        error stop "transfer 1.0d0 high word mismatch"

    ! Array constructor with ieee_value
    dpnan = ieee_value(dpnan, [ieee_signaling_nan, ieee_quiet_nan])
    if (.not. ieee_is_nan(dpnan(1))) &
        error stop "dpnan(1) should be NaN"
    if (.not. ieee_is_nan(dpnan(2))) &
        error stop "dpnan(2) should be NaN"
    ibits64 = transfer(dpnan(2), 0_int64)
    if (ibits64 /= int(z'7FF8000000000000', int64)) &
        error stop "array ieee_value qnan mismatch"
end program test_ieee_nan_value
