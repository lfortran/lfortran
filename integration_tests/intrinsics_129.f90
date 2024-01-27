program intrinsics_129
    integer(4) :: x, y
    x = 16
    y = 2

    print*, rshift(-10_8, 1)
    print*, shiftr(-10_8, 1)

    print*, rshift(-x, y)
    print*, shiftr(-x, y)
end