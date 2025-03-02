program type_parameter_inquiry_01
    real(kind=8) :: real8
    complex :: c
    character(len=10) :: s
    integer(kind=1) :: iarr(5)
    character(len=1) :: carr(5)

    iarr = [1, 2, 3, 4, 5]
    carr = [character(len=1) :: "a", "b", "c", "d", "e"]

    if (s%len /= 10 .or. s%len /= len(s)) error stop
    if (carr%len /= 1 .or. carr%len /= len(carr)) error stop
    if (carr(2)%len /= 1 .or. carr(2)%len /= len(carr(2))) error stop

    if (real8%kind /= 8 .or. real8%kind /= kind(real8)) error stop
    if (c%kind /= 4 .or. c%kind /= kind(c)) error stop
    if (iarr%kind /= 1 .or. iarr%kind /= kind(iarr)) error stop
    if (iarr(3)%kind /= 1 .or. iarr(3)%kind /= kind(iarr(3))) error stop
end program type_parameter_inquiry_01
