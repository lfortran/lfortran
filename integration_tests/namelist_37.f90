program namelist_37
    implicit none

    type :: inner
        integer :: a
        integer :: b
    end type inner

    type :: outer
        type(inner) :: child
    end type outer

    type(outer) :: items(2)

    namelist /settings/ items

    items = [outer(inner(0, 0)), outer(inner(0, 0))]

    open(10, status="scratch")
    write(10, "(a)") "&settings items=1,2,3,4 /"
    rewind(10)
    read(10, nml=settings)

    if (items(1)%child%a /= 1) error stop
    if (items(1)%child%b /= 2) error stop
    if (items(2)%child%a /= 3) error stop
    if (items(2)%child%b /= 4) error stop
end program namelist_37
