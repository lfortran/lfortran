program file_34
    implicit none
    integer, allocatable :: x
    integer :: fh
    allocate(x)
    open(newunit=fh, status="scratch")
    write(fh, *) 666 
    rewind fh
    read(fh, *) x
    print "(I0)", x
    close(fh)
    if (x /= 666) error stop
    deallocate(x)
end program file_34