program allocate_23
    implicit none

    type :: base
        integer :: x = 100
    end type

    type, extends(base) :: derived
        integer :: y = 200
    end type

    type(base), allocatable :: a1, a2
    type(derived), allocatable :: d1
    class(base), allocatable :: c1

    ! type -> type
    allocate(a1)
    a1%x = 11
    allocate(a2, source=a1)
    if (a2%x /= 11) error stop

    ! type -> class
    allocate(d1)
    d1%x = 22
    d1%y = 44
    allocate(c1, source=d1)
    select type(c1)
        type is (derived)
            if (c1%x /= 22 .or. c1%y /= 44) error stop
        class default
            error stop
    end select

end program allocate_23
