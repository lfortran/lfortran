program test_ichar

    implicit none
    ! Compile_time
    integer :: i, j, c
    integer(8) :: li_1, lj_1, li_2, lj_2
    character(len=1) :: c_1 = 'b'
    character(len = 1) :: a = 'a'
    character(len = 1) :: b = 'b'
    character(len = 1) :: d = '#'
    character(len=1) :: e(3)
    integer :: x(3)

    i = ichar(' ')
    j = iachar(' ')
    li_1 = ichar('A', 8)
    lj_1 = iachar('a', 8)
    li_2 = ichar('Z', kind=8)
    lj_2 = iachar('z', kind=8)
    c_1 = char(100);
    e = ["a", "b", "c"]
    x = [97, 98, 99]

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
    c_1 = achar(100)
    if (c_1 /= "d") error stop
    c_1 = achar(c)
    if (c_1 /= "d") error stop
    c_1 = char(100)
    if (c_1 /= "d") error stop
    c_1 = char(c)
    if (c_1 /= "d") error stop

    c_1 = "e"
    c = iachar("e")
    if (c /= 101) error stop
    c = iachar(c_1)
    if (c /= 101) error stop
    c = ichar("e")
    if (c /= 101) error stop
    c = ichar(c_1)
    if (c /= 101) error stop

    print *, ichar(a)
    if(ichar(a) /= 97) error stop

    print *, ichar(b)
    if(ichar(b) /= 98) error stop

    print *, ichar(d)
    if(ichar(d) /= 35) error stop

    print *, ichar('a')
    if(ichar('a') /= 97) error stop

    print *, ichar('b')
    if(ichar('b') /= 98) error stop

    print *, ichar('C')
    if(ichar('C') /= 67) error stop

    print *, ichar('#')
    if(ichar('#') /= 35) error stop

    print *, kind(ichar(a))
    if(kind(ichar(a)) /= 4) error stop

    print *, kind(ichar(a, 8))
    if(kind(ichar(a, 8)) /= 8) error stop

    print *, kind(ichar('a'))
    if(kind(ichar('a')) /= 4) error stop

    print *, kind(ichar('a', 8))
    if(kind(ichar('a', 8)) /= 8) error stop

    print *, ichar(e)
    if (any(ichar(e) /= x)) error stop


end program test_ichar
