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
    if (iol /= size(data) * 4) error stop

    open(10, status="scratch", form="formatted")
    write(10, "(*(i5))") (data(i), i = 1, size(data), 2)

    rewind(10)
    i = -42
    read(10, "(*(i5))") (data(i), i = 1, size(data), 2)
    if (i /= size(data) + 2) error stop

    close(10)

    i = -42
    inquire(iolength=iol) (data(i), i = 1, size(data), 2)
    if (i /= size(data) + 2) error stop
    if (iol /= 6 * 4) error stop

    open(10, status="scratch", form="formatted")
    write(10, "(*(i5))") (data(i), i = size(data), 1, -2)

    rewind(10)
    i = -42
    read(10, "(*(i5))") (data(i), i = size(data), 1, -2)
    if (i /= -1) error stop

    close(10)

    i = -42
    inquire(iolength=iol) (data(i), i = size(data), 1, -2)
    if (i /= -1) error stop
    if (iol /= 6 * 4) error stop
end program implied_do_loops38
