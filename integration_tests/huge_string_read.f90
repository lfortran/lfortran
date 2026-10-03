program huge_string_read
    implicit none
    double precision :: x
    character(len=40) :: xstring

    x = huge(1d0)
    write(xstring,*) x
    read(xstring,*) x

    if (x /= huge(1d0)) error stop

end program huge_string_read