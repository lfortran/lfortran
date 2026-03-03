program transfer_15
    use, intrinsic :: iso_fortran_env, only: int64, int32
    implicit none
    integer(int64) :: key(0:15)
    integer(int32) :: seed(2)
    integer :: i

    key = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
    seed(1:2) = transfer(key(1), 0_int32, 2)

    if (seed(1) /= 2) error stop
    if (seed(2) /= 0) error stop

    print *, "PASS"
end program transfer_15