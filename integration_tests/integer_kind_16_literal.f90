program integer_kind_16_literal
    implicit none
    integer(16) :: a, b, c, d
    a = 170141183460469231731687303715884105727_16
    b = -170141183460469231731687303715884105727_16
    c = 1180591620717411303424_16
    d = a + 1_16
    if (a /= huge(0_16)) error stop 1
    if (b /= -a) error stop 2
    if (c /= 2_16 ** 70) error stop 3
    if (d /= -huge(0_16) - 1) error stop 4
    if (c / 2_16 /= 590295810358705651712_16) error stop 5
    if (.not. (a > c)) error stop 6
    if (c * 2_16 /= 2361183241434822606848_16) error stop 7
    print *, a
    print *, b
    print *, c
end program
