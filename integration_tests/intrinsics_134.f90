program intrinsics_134

    character(4), parameter :: y = adjustl("okay")
    character(len=5) :: x
    character(len=20) :: str = '   gfortran'
    str = adjustl(str)
    print *, str
    if (str /= 'gfortran') error stop
    str = adjustl('   gfortran')
    print *, str
    if (str /= 'gfortran') error stop

    print *, "|"//adjustl("       abc")//"|"
    if ("|"//adjustl("       abc")//"|" /= '|abc       |') error stop

    x = "     "
    print *, adjustl(x)
    print *, adjustl("     ")
    if (adjustl("     ") /= "     ") error stop
    if (adjustl(x) /= "     ") error stop
    if (y /= "okay") error stop
    
end program
