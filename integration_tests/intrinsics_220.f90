program intrinsics_220
    integer :: a, b, i
    integer(8) :: c, d, e
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

    print *, merge_bits(-10, -15, -2)
    if(merge_bits(-10, -15, -2) /= -9) error stop

    print *, merge_bits(1005627, 1027293, 10098)
    if(merge_bits(1005627, 1027293, 10098) /= 1018047) error stop

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

    print *, merge_bits(10, 15, 2)
    if(merge_bits(10, 15, 2) /= 15) error stop

    print *, merge_bits(-1005627827, 1026271812, -1628291)
    if(merge_bits(-1005627827, 1026271812, -1628291) /= -1005613491) error stop

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

    print *, merge_bits(-10, 15, -2)
    if(merge_bits(-10, 15, -2) /= -9) error stop

    print *, merge_bits(92748272, -82927282, -10098)
    if(merge_bits(92748272, -82927282, -10098) /= 92748224) error stop

end program