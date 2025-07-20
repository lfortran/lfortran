  program read_04
    implicit none
    integer, allocatable :: x
    integer :: fh
    allocate(x)
    open(newunit=fh, status="scratch")
      write(fh, *) 666
      rewind fh 
      read(fh, *) x
    close(fh)
    if (x /= 666) error stop
    deallocate(x)
  end program read_04