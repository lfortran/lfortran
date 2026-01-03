program intrinsics_289
    implicit none
    integer(4), parameter :: i1 = ishftc(1, 3)
    integer(8), parameter :: i2 = ishftc(5_8, -1_8)
    integer(4), parameter :: ar1(3) = ishftc([11, 2, 23], 1)
    integer(8), parameter :: ar2(3) = ishftc([1_8, 21_8, 3_8], -2_8)
    integer(kind=1) :: res_1
    integer(kind=2) :: res_2
    integer(kind=4) :: res_4
    integer(kind=8) :: res_8
    integer(4) :: i3 = 7
    integer(8) :: i4 = 12
    integer(4) :: ar3(3) = [22, 4, 46]
    integer(8) :: ar4(3) = [4611686018427387904_8, 4611686018427387909_8, -4611686018427387904_8]
    integer :: shift = 3
 
    print *, i1
    if (i1 /= 8) error stop
    print *, i2
    if (i2 /= -9223372036854775806_8) error stop
    print *, ar1
    if (any(ar1 /= [22, 4, 46])) error stop
    print *, ar2
    if (any(ar2 /= [4611686018427387904_8, 4611686018427387909_8, -4611686018427387904_8])) error stop

    ! Does not work with LFortran yet

    ! print *, ishftc(i3, shift)
    ! if (ishftc(i3, shift) /= 56) error stop
    ! print *, ishftc(i4, -shift)
    ! if (ishftc(i4, -shift) /= -9223372036854775807_8) error stop
    ! print *, ishftc(ar3, shift)
    ! if (any(ishftc(ar3, shift) /= [176, 32, 368])) error stop
    ! print *, ishftc(ar4, -shift)
    ! if (any(ishftc(ar4, -shift) /= [576460752303423488_8, -6341068275337658368_8, 1729382256910270464_8])) error stop

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
