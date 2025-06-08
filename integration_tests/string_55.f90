program string_55
    character(len=20) :: str = 'gfortran   '
    str = adjustr(str)
    print *, str
    print "(a)", str
    ! 12 EMPTY CHARACTERS + "gfortran"
    if(str /= "            gfortran") error stop
end program