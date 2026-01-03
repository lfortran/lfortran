program intrinsics_142
    integer, dimension(3) :: a, b
    integer(8), dimension(5) :: c, d
    integer(8), dimension(3) :: e
    integer :: res
    integer(8) :: res_8
    a = [1, 2, 3]
    b = [4, 5, 6]
    c = [1_8, 2_8, 3_8, 4_8, 5_8]
    d = [6_8, 7_8, 8_8, 9_8, 10_8]
    e = [6_8, 7_8, 8_8]

    res = dot_product([1, 2, 3],[4, 5, 6])
    print *, res
    if (res /= 32) error stop

    res = dot_product(a, b)
    print *, res
    if (res /= 32) error stop

    res_8 = dot_product([1_8, 2_8, 3_8, 4_8, 5_8],[6_8, 7_8, 8_8, 9_8, 10_8])
    print *, res_8
    if (res_8 /= 130) error stop

    res_8 = dot_product(c, d)
    print *, res_8
    if (res_8 /= 130) error stop

    res_8 = dot_product(a, e)
    print *, res_8
    if (res_8 /= 44) error stop

    res_8 = dot_product(e, b)
    print *, res_8
    if (res_8 /= 107) error stop

end program intrinsics_142
