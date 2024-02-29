program intrinsics_170
    character(len = 1) :: a = 'a'
    character(len = 1) :: b = 'b'
    character(len = 1) :: C = 'C'
    character(len = 1) :: d = '#'

    print *, ichar(a)
    if(ichar(a) /= 97) error stop

    print *, ichar(b)
    if(ichar(b) /= 98) error stop
    
    print *, ichar(C)
    if(ichar(C) /= 67) error stop

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
end program
