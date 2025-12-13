program file_37
    implicit none
    integer, allocatable :: x
    integer :: fh

    ! Open the file for writing and reading
    allocate(x)
    open(newunit=fh, status="scratch")
    write(fh, *) 666
    rewind(fh)
    read(fh, *) x
    print "(I0)", x
    close(fh)
    ! deallocate(x)
end program file_37
