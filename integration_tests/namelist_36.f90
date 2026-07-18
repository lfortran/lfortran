program namelist_36
    implicit none

    type :: pair
        integer :: a
        integer :: b
    end type pair

    type(pair) :: items(2)

    namelist /settings/ items

    items = [pair(1, 2), pair(3, 4)]

    open(10, status="scratch")
    write(10, nml=settings)
    rewind(10)

    items = [pair(0, 0), pair(0, 0)]
    read(10, nml=settings)

    if (items(1)%a /= 1) error stop
    if (items(1)%b /= 2) error stop
    if (items(2)%a /= 3) error stop
    if (items(2)%b /= 4) error stop
    close(10)

    open(11, status="scratch")
    write(11, "(a)") "&settings"
    write(11, "(a)") " items = 1, 2, 3, 4"
    write(11, "(a)") "/"
    rewind(11)

    items = [pair(0, 0), pair(0, 0)]
    read(11, nml=settings)

    if (items(1)%a /= 1) error stop
    if (items(1)%b /= 2) error stop
    if (items(2)%a /= 3) error stop
    if (items(2)%b /= 4) error stop
    close(11)
end program namelist_36
