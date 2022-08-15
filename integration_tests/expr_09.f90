program expr_09
implicit none
    integer :: x4, y4
    integer(kind=8) :: x8, y8

    y4 = 5
    x4 = y4 ** 2
    print *, y4, x4

    if (x4 /= 25) error stop

    y8 = 5
    x8 = y8**2
    print *, y8, x8
    if (x8 /= 25) error stop

end program
