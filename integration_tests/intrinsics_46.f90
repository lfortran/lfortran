program test_ichar

    implicit none
    ! Compile_time
    integer :: i, j, c
    integer(8) :: li_1, lj_1, li_2, lj_2
    character(len=1) :: c_1 = 'b'

    i = ichar(' ')
    j = iachar(' ')
    li_1 = ichar('A', 8)
    lj_1 = iachar('a', 8)
    li_2 = ichar('Z', kind=8)
    lj_2 = iachar('z', kind=8)
    c_1 = char(100);

    if (i /= 32) error stop
    if (j /= 32) error stop
    if (li_1 /= 65) error stop
    if (lj_1 /= 97) error stop
    if (li_2 /= 90) error stop
    if (lj_2 /= 122) error stop

    ! Run_time
    c = ichar(c_1)
    if (c /= 100) error stop
    if (char(c) /= 'd') error stop

    c = 100
    c_1 = achar(c)
    if (c_1 /= "d") error stop

end program test_ichar
