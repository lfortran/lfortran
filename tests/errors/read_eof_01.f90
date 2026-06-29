program read_eof_01
    implicit none
    integer :: i
    open(10, status="scratch")
    read(10, '(i4)') i
end program read_eof_01
