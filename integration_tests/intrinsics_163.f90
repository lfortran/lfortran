program intrinsics_163

    integer :: x, y, size1
    integer(8) :: a, b, size2

    integer(kind=1) :: res_1
    integer(kind=2) :: res_2
    integer(kind=4) :: res_4
    integer(kind=8) :: res_8

    integer(kind=1), parameter :: para_1 = ishftc(10_1, -2_1)
    integer(kind=2), parameter :: para_2 = ishftc(10_2, -2_2)
    integer(kind=4), parameter :: para_4 = ishftc(10_4, -2_4)
    integer(kind=8), parameter :: para_8 = ishftc(10_8, -2_8)
 
    if (para_1 /= -126) error stop
    if (para_2 /= -32766) error stop
    if (para_4 /= -2147483646) error stop
    if (para_8 /= -9223372036854775806_8) error stop

    res_1 = ishftc(10_1, 2_1)
    print *, res_1
    if (res_1 /= 40) error stop
 
    res_1 = ishftc(10_1, -2_1)
    print *, res_1
    if (res_1 /= -126) error stop
 
    res_1 = ishftc(-10_1, 2_1)
    print *, res_1
    if (res_1 /= -37) error stop
 
    res_1 = ishftc(-10_1, -2_1)
    print *, res_1
    if (res_1 /= -67) error stop
 
    res_2 = ishftc(10_2, 2_2)
    print *, res_2
    if (res_2 /= 40) error stop
 
    res_2 = ishftc(10_2, -2_2)
    print *, res_2
    if (res_2 /= -32766) error stop
 
    res_2 = ishftc(-10_2, 2_2)
    print *, res_2
    if (res_2 /= -37) error stop
 
    res_2 = ishftc(-10_2, -2_2)
    print *, res_2
    if (res_2 /= -16387) error stop
 
    res_4 = ishftc(10_4, 2_4)
    print *, res_4
    if (res_4 /= 40) error stop
 
    res_4 = ishftc(10_4, -2_4)
    print *, res_4
    if (res_4 /= -2147483646) error stop
 
    res_4 = ishftc(-10_4, 2_4)
    print *, res_4
    if (res_4 /= -37) error stop
 
    res_4 = ishftc(-10_4, -2_4)
    print *, res_4
    if (res_4 /= -1073741827) error stop
 
    res_8 = ishftc(10_8, 2_8)
    print *, res_8
    if (res_8 /= 40_8) error stop
 
    res_8 = ishftc(10_8, -2_8)
    print *, res_8
    if (res_8 /= -9223372036854775806_8) error stop
 
    res_8 = ishftc(-10_8, 2_8)
    print *, res_8
    if (res_8 /= -37_8) error stop
 
 
    res_8 = ishftc(-10_8, -2_8)
    print *, res_8
    if (res_8 /= -4611686018427387907_8) error stop
 
    res_1 = ishftc(127_1, 7_1)
    print *, res_1
    if (res_1 /= -65) error stop
 
    res_1 = ishftc(127_1, -7_1)
    print *, res_1
    if (res_1 /= -2) error stop
 
    res_1 = ishftc(-127_1, 7_1)
    print *, res_1
    if (res_1 /= -64) error stop
 
    res_1 = ishftc(-127_1, -7_1)
    print *, res_1
    if (res_1 /= 3) error stop
 
    res_2 = ishftc(32767_2, 15_2)
    print *, res_2
    if (res_2 /= -16385) error stop
 
    res_2 = ishftc(32767_2, -15_2)
    print *, res_2
    if (res_2 /= -2) error stop
 
    res_2 = ishftc(-32767_2, 15_2)
    print *, res_2
    if (res_2 /= -16384) error stop
 
    res_2 = ishftc(-32767_2, -15_2)
    print *, res_2
    if (res_2 /= 3) error stop
 
    res_4 = ishftc(2147483647_4, 31_4)
    print *, res_4
    if (res_4 /= -1073741825) error stop
 
    res_4 = ishftc(2147483647_4, -31_4)
    print *, res_4
    if (res_4 /= -2) error stop
 
    res_4 = ishftc(-2147483647_4, 31_4)
    print *, res_4
    if (res_4 /= -1073741824) error stop
 
    res_4 = ishftc(-2147483647_4, -31_4)
    print *, res_4
    if (res_4 /= 3) error stop
 
    res_8 = ishftc(9223372036854775807_8, 63_8)
    print *, res_8
    if (res_8 /= -4611686018427387905_8) error stop
 
    res_8 = ishftc(9223372036854775807_8, -63_8)
    print *, res_8
    if (res_8 /= -2) error stop
 
    res_8 = ishftc(-9223372036854775807_8, 63_8)
    print *, res_8
    if (res_8 /= -4611686018427387904_8) error stop
 
    res_8 = ishftc(-9223372036854775807_8, -63_8)
    print *, res_8
    if (res_8 /= 3) error stop

    res_1 = ishftc(10_1, 2_1, 4_1)
    print *, res_1
    if (res_1 /= 10) error stop
 
    res_1 = ishftc(10_1, -2_1, 7_1)
    print *, res_1
    if (res_1 /= 66) error stop
 
    res_2 = ishftc(10_2, 2_2, 6_2)
    print *, res_2
    if (res_2 /= 40) error stop
 
    res_2 = ishftc(10_2, -2_2, 11_2)
    print *, res_2
    if (res_2 /= 1026) error stop

    res_4 = ishftc(10_4, 2_4, 13_4)
    print *, res_4
    if (res_4 /= 40) error stop
 
    res_4 = ishftc(10_4, -2_4, 31_4)
    print *, res_4
    if (res_4 /= 1073741826) error stop

 
    res_8 = ishftc(10_8, 2_8, 62_8)
    print *, res_8
    if (res_8 /= 40_8) error stop
 
    res_8 = ishftc(10_8, -2_8, 59_8)
    print *, res_8
    if (res_8 /= 288230376151711746_8) error stop

    x = 17
    y = 4
    size1 = 6

    a = 8
    b = 2
    size2 = 4

    print *, ishftc(a, b, size2)
    if (ishftc(a, b, size2) /= 2) error stop

    print *, ishftc(x, y, size1)
    if (ishftc(x, y, size1) /= 20) error stop

    a = 293436938_8
    res_8 = ishftc(a, 23)
    print *, res_8
    if (res_8 /= 2461527445602304_8) error stop 
    b = -1292093_8
    res_8 = ishftc(b, -32)
    print *, res_8
    if (res_8 /= -5549492883423233_8) error stop

 end program
