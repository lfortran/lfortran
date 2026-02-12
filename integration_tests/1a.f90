program nested_example
    use, intrinsic :: iso_fortran_env, only: int64, int8, int32
    implicit none
    integer(int64) :: key(0:15) = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
    integer(int32) :: seed(2)
    seed(1:2) = transfer(key(1), 0_int32, 2)
    print *, seed
end program nested_example