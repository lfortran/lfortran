program array_07_transfer
    use, intrinsic :: iso_fortran_env, only: int32, int64, real32, int8
    integer(int8) :: key(16) = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
    integer(int64) :: map_to_6

    map_to_6 = transfer( [ key(1), 0_int8, 0_int8, 0_int8, &
                        0_int8, 0_int8, 0_int8, 0_int8 ], &
                        0_int64 )
    print *, map_to_6
    if (map_to_6 /= 1) error stop
    map_to_6 = transfer( [ key(1:4), 0_int8, 0_int8, 0_int8, 0_int8], &
                        0_int64 )
    print *, map_to_6
    if (map_to_6 /= 67305985) error stop
end program array_07_transfer