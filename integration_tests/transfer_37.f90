program transfer_37
    ! transfer of an integer(8) scalar to an integer(4) array section
    use, intrinsic :: iso_fortran_env, only: int64, int8, int32
    implicit none
    integer(int64) :: key(0:15) = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
    integer(int32) :: seed(2)
    integer(int32) :: lo, hi
    seed(1:2) = transfer(key(1), 0_int32, 2)
    print *, seed
    if (transfer(1_int32, 0_int8) == 1_int8) then
        lo = 1; hi = 2
    else
        lo = 2; hi = 1
    end if
    if (seed(lo) /= 2) error stop
    if (seed(hi) /= 0) error stop
    if (transfer(seed, 0_int64) /= key(1)) error stop
end program transfer_37
