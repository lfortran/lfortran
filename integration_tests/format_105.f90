program format_105
    implicit none
    integer :: u
    character(len=80) :: line

    open(newunit=u, file="format_hollerith_01_output.txt", status="replace")

    write(u, 10)

    close(u)

    open(newunit=u, file="format_hollerith_01_output.txt", status="old")

    read(u, '(A)') line
    if (len_trim(line) /= 0) error stop

    read(u, '(A)') line
    if (len_trim(line) /= 51) error stop
    if (line(1:1) /= ' ') error stop
    if (line(2:51) /= repeat('-', 50)) error stop
    close(u, status="delete")

    print *, "All tests passed"

10 format(/,1x,50(1h-))
end program format_105