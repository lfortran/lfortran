program transfer_19
use, intrinsic :: iso_fortran_env, only: int32, int64, int8
integer(int8) :: key(16) = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
integer(int64) :: buf(2)

buf = transfer( key, 0_int64, 2 )
if (buf(1) /= 578437695752307201_int64) error stop
if (buf(2) /= 1157159078456920585_int64) error stop

call test_sub(key)
contains
subroutine test_sub(key)
    integer(int64) :: bend = 1
    integer(int8), intent(in), target :: key(0:)
    integer(int64) :: buf2(0:1)
    buf2(0:2*bend-1) = transfer( key(0:16_8*bend-1_8), 0_int64, 2*bend )
    if (buf2(0) /= 578437695752307201_int64) error stop
    if (buf2(1) /= 1157159078456920585_int64) error stop
end subroutine
end program transfer_19