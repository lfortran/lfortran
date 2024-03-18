program intrinsics_91
    character(3) :: abc = "abc"
    character(8) :: elephant = "elephant"
    character(6) :: potato = "potato"
    character(1) :: b = "b"
    character(5) :: an = "@a!n#"
    character(1) :: t = "t"
    character(2) :: ta = "ta"

    print*, index("abc", "b")
    if ( index("abc", "b") /= 2 ) error stop
    print*, index("elephant", "@a!n#")
    if ( index("elephant", "@a!n#") /= 0 ) error stop
    print*, index("potato", "t", back=.true.)
    if ( index("potato", "t", back=.true.) /= 5 ) error stop
    print*, index("potato", "t", back=.false.)
    if ( index("potato", "t", back=.false.) /= 3 ) error stop
    print*, index("potato", "ta", back=.true.)
    if ( index("potato", "ta", back=.true.) /= 3 ) error stop

    print*, index(abc, b)
    if ( index(abc, b) /= 2 ) error stop
    print*, index(elephant, an)
    if ( index(elephant, an) /= 0 ) error stop
    print*, index(potato, t, back=.true.)
    if ( index(potato, t, back=.true.) /= 5 ) error stop
    print*, index(potato, t, back=.false.)
    if ( index(potato, t, back=.false.) /= 3 ) error stop
    print*, index(potato, ta, back=.true.)
    if ( index(potato, ta, back=.true.) /= 3 ) error stop

end program
