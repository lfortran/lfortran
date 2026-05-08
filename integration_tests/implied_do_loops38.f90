program implied_do_loops38
    implicit none

    integer :: data(11)
    integer :: i, iol

    data = [(i, i = 1, size(data))]

    open(10, status="scratch", form="formatted")
    write(10, "(*(i5))") (data(i), i = 1, size(data))

    rewind(10)
    i = -42
    read(10, "(*(i5))") (data(i), i = 1, size(data))
    if (i /= size(data) + 1) error stop

    close(10)

    i = -42
    inquire(iolength=iol) (data(i), i = 1, size(data))
    if (i /= size(data) + 1) error stop
end program implied_do_loops38
