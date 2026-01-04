program read_15
    implicit none
    integer :: u, x
    character(len=*), parameter :: fname = "read_15_tmp.txt"

    open(newunit=u, file=fname, status="replace", action="write")
    write(u, *) 1
    close(u)

    open(newunit=u, file=fname, status="old", action="read")
    read(u, *, end=10) x
    if (x /= 1) then
        print *, "FAIL"
        error stop
    end if

    read(u, *, end=10) x
    print *, "FAIL"
    error stop

10  continue
    print *, "PASS"
    close(u, status="delete")
end program read_15
