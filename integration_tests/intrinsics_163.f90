program intrinsics_163
    integer(kind=1) :: res_1
    integer(kind=2) :: res_2
    integer(kind=4) :: res_4
    integer(kind=8) :: res_8
 
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
 
 
 end program
