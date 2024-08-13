program intrinsics_177

    character(4), parameter :: y = adjustr("okay")
    character(len=5) :: x
    character(len=21) :: str = 'gfortran   '
    str = adjustr(str)
    print *, str
    if (str /= '             gfortran') error stop
    str = adjustr('gfortran   ')
    print *, str
    if (str /= '   gfortran') error stop

    print *, "|"//adjustr("abc       ")//"|"
    if ("|"//adjustr("abc       ")//"|" /= '|       abc|') error stop

    x = "     "
    print *, adjustr(x)
    print *, adjustr("     ")
    if (adjustr("     ") /= "     ") error stop
    if (adjustr(x) /= "     ") error stop
    if (y /= "okay") error stop

end program
