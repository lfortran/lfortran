program integer_kind_16_read
    implicit none
    integer(16) :: x, y
    integer :: unit
    character(len=80) :: s, line
    s = "170141183460469231731687303715884105727"
    read(s, *) x
    if (x /= huge(0_16)) error stop 1
    s = "  -42"
    read(s, '(i5)') y
    if (y /= -42) error stop 2
    s = "FF"
    read(s, '(z2)') x
    if (x /= 255) error stop 3
    s = "11111111"
    read(s, '(b8)') x
    if (x /= 255) error stop 4
    s = "377"
    read(s, '(o3)') x
    if (x /= 255) error stop 5
    open(newunit=unit, status='scratch')
    write(unit, *) huge(0_16)
    rewind(unit)
    read(unit, *) x
    if (x /= huge(0_16)) error stop 6
    rewind(unit)
    write(unit, '(i40)') -huge(0_16)
    rewind(unit)
    read(unit, '(i40)') y
    if (y /= -huge(0_16)) error stop 7
    close(unit)
    print *, x, y
end program
