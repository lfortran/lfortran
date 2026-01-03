program intrinsics_91
    character(3) :: abc = "abc"
    character(8) :: elephant = "elephant"
    character(6) :: potato = "potato"
    character(1) :: b = "b"
    character(5) :: an = "@a!n#"
    character(1) :: t = "t"
    character(2) :: ta = "ta"

    integer, parameter :: i1 = index("abc", "b")
    integer, parameter :: i2 = index("elephant", "@a!n#")
    integer, parameter :: i3 = index("potato", "t", back=.true.)
    integer, parameter :: i4 = index("potato", "t", back=.false.)
    integer, parameter :: i5 = index("potato", "ta", back=.true.)

    integer, parameter :: ar1(3) = index(["abc", "ele", "pot"], "b")
    integer, parameter :: ar2(3) = index(["elephant", "gfortran", "lfortran"], "@a!n#")

    character(3) :: arr1(3)
    character(8) :: arr2(3)
    integer :: res(3)

    print*, i1
    if ( i1 /= 2 ) error stop
    print*, i2
    if ( i2 /= 0 ) error stop
    print*, i3
    if ( i3 /= 5 ) error stop
    print*, i4
    if ( i4 /= 3 ) error stop
    print*, i5
    if ( i5 /= 3 ) error stop

    print*, ar1
    if ( any(ar1 /= [2, 0, 0]) ) error stop
    print*, ar2
    if ( any(ar2 /= [0, 0, 0]) ) error stop
    
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

    arr1 = ["abc", "ele", "pot"]
    arr2 = ["elephant", "gfortran", "lfortran"]

    res = index(arr1, b)
    print*, res
    if ( any(res /= [2, 0, 0]) ) error stop
    res = index(arr2, an, back = .true.)
    print*, res
    if ( any(res /= [0, 0, 0]) ) error stop

end program
