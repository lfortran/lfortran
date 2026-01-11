program test_inquire_size
    implicit none
    integer :: fh
    integer :: length
    character(*), parameter :: test_lines = 'build.f90'
    open(newunit=fh, file="test_inquire_size_test.txt", access="stream", form='unformatted', status='replace')
    write(fh) test_lines
    inquire(fh, size=length)
    close(fh)
    print *, "File size =", length, "bytes"
    if(length /= 9) error stop
end program test_inquire_size
