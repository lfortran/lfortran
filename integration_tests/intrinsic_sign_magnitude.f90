program intrinsic_sign_magnitude
    implicit none
    integer(8) :: a, p
    integer(2) :: small
    a = -9007199254740993_8
    p = 3
    if (sign(a, p) /= 9007199254740993_8) error stop 1
    p = 0
    if (sign(a, p) /= 9007199254740993_8) error stop 2
    p = -3
    if (sign(a, p) /= -9007199254740993_8) error stop 3
    small = -1234_2
    if (sign(small, 0_2) /= 1234_2) error stop 4
end program
