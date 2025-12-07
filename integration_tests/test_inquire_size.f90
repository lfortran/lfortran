program test_inquire_size
    implicit none
    integer :: fh
    integer :: length
    character(*), parameter :: test_lines = 'build.f90'
    open(newunit=fh, file="test", access="stream", form='unformatted')
    write(fh) test_lines
    inquire(fh, size=length)
    close(fh)
    print *, "File size =", length, "bytes"
end program test_inquire_size
