program transfer_35
    ! transfer of an integer(4) array to integer(1) with a size argument
    implicit none

    integer(1), parameter :: int8_0 = 0

    integer(4)            :: rand_object(4) = [1, 2, 3, 4]
    integer(1)            :: test_8_bits(16)
    integer(1)            :: expected(16)
    integer(4)            :: back(4)
    logical               :: little_endian

    little_endian = transfer(1_4, int8_0) == 1_1
    if (little_endian) then
        expected = [1_1, 0_1, 0_1, 0_1, 2_1, 0_1, 0_1, 0_1, &
                    3_1, 0_1, 0_1, 0_1, 4_1, 0_1, 0_1, 0_1]
    else
        expected = [0_1, 0_1, 0_1, 1_1, 0_1, 0_1, 0_1, 2_1, &
                    0_1, 0_1, 0_1, 3_1, 0_1, 0_1, 0_1, 4_1]
    end if

    test_8_bits(:) = transfer( rand_object, int8_0, 16 )
    print *, test_8_bits
    if (any(test_8_bits /= expected)) error stop
    back = transfer(test_8_bits, 0_4, 4)
    if (any(back /= rand_object)) error stop
end program transfer_35
