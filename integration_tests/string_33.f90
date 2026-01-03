program string_33
    implicit none
    character(len=1) :: str

    write(str, '(a)') "A"
    print *, "|"//str//"|"

    if (str /= "A") error stop

end program
