program write_44
    implicit none
    integer(1) :: u

    u = 10_1
    write(10_1, *) "x"
    write(u, *) "x"
    print *, "ok"
end program
