program character_max_parameter_01
    implicit none

    character(len=6), parameter :: c1(3) = max( &
        ["str1  ", "str2  ", "char  "], &
        ["!str#$", "xtring", "charac"])

    character(len=4), parameter :: c2(3) = max( &
        ["str1", "str2", "char"], &
        ["!str", "xtri", "abcd"])

    character(len=3), parameter :: c3(2) = min( &
        ["zzz", "aaa"], ["mmm", "bbb"])

    character(len=5), parameter :: c4(3) = max( &
        ["hello", "world", "abcde"], &
        ["apple", "zebra", "abcdf"])

    character(len=3), parameter :: c5(4) = &
        ["hel", "wor", "tes", "abc"]

    character(len=8), parameter :: c6(2) = &
        ["hi      ", "go      "]

    if (c1(1) /= "str1  ") error stop
    if (c1(2) /= "xtring") error stop
    if (c1(3) /= "charac") error stop

    if (c2(1) /= "str1") error stop
    if (c2(2) /= "xtri") error stop
    if (c2(3) /= "char") error stop

    if (c3(1) /= "mmm") error stop
    if (c3(2) /= "aaa") error stop

    if (c4(1) /= "hello") error stop
    if (c4(2) /= "zebra") error stop
    if (c4(3) /= "abcdf") error stop

    if (c5(1) /= "hel") error stop
    if (c5(2) /= "wor") error stop
    if (c5(3) /= "tes") error stop
    if (c5(4) /= "abc") error stop

    if (c6(1) /= "hi      ") error stop
    if (c6(2) /= "go      ") error stop
end program
