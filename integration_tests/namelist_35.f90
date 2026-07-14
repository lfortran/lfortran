program namelist_33
    implicit none

    type :: pair
        integer :: value
    end type pair

    type(pair) :: items(2)

    namelist /settings/ items

    items = [pair(4), pair(9)]

    open(10, status="scratch")
    write(10, nml=settings)
    rewind(10)

    items = [pair(0), pair(0)]
    read(10, nml=settings)

    if (items(1)%value /= 4) error stop
    if (items(2)%value /= 9) error stop
    print *, "derived namelist array ok"
end program namelist_33
