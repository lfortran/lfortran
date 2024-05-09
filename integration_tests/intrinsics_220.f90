program intrinsics_220
    integer :: a, b, i
    integer(8) :: c, d, e
    integer(4) :: expected_1(3)
    integer(8) :: expected_2(3)
    integer :: iterator

    integer(4), parameter :: result_1(3) = merge_bits([-10, 10, -10], [-15, 15, 15], [-2, 2, -2])

    integer(8), parameter :: result_2(3) = merge_bits([1005627, -1005627827, 92748272], &
    [1027293, 1026271812, -82927282], [10098, -1628291, -10098])

    expected_1 = [-9, 15, -9]
    expected_2 = [1018047_8, -1005613491_8, 92748224_8]


    do iterator = 1, size(result_1)
        print *, result_1(iterator)
        if (result_1(iterator) /= expected_1(iterator)) error stop
    end do

    do iterator = 1, size(result_2)
        print *, result_2(iterator)
        if (result_2(iterator) /= expected_2(iterator)) error stop
    end do

    a = -10
    b = -15
    i = -2

    c = 1005627
    d = 1027293
    e = 10098

    print *, merge_bits(a, b, i)
    if(merge_bits(a, b, i) /= -9) error stop

    print *, merge_bits(c, d, e)
    if(merge_bits(c, d, e) /= 1018047) error stop

    a = 10
    b = 15
    i = 2

    c = -1005627827
    d = 1026271812
    e = -1628291

    print *, merge_bits(a, b, i)
    if(merge_bits(a, b, i) /= 15) error stop    

    print *, merge_bits(c, d, e)
    if(merge_bits(c, d, e) /= -1005613491) error stop

    a = -10
    b = 15
    i = -2

    c = 92748272
    d = -82927282
    e = -10098

    print *, merge_bits(a, b, i)
    if(merge_bits(a, b, i) /= -9) error stop

    print *, merge_bits(c, d, e)
    if(merge_bits(c, d, e) /= 92748224) error stop

end program