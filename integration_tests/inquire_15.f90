program inquire_15
    implicit none

    integer(8) :: unit_number
    integer(8) :: position

    unit_number = 21_8
    open(unit=unit_number, file="inquire_15.tmp", access="stream", status="replace")
    write(unit_number) "abcd"
    inquire(unit=unit_number, pos=position)
    close(unit_number, status="delete")

    if (position /= 5_8) error stop 1
    print *, "stream position ok"
end program
