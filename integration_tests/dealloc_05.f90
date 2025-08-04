program dealloc_05
    implicit none

    type :: base
        integer :: x
    end type

    type, extends(base) :: derived
        real :: r
    end type

    class(base), allocatable :: var

    allocate(derived :: var)

    select type(var)
        type is (derived)
        class default
            error stop
    end select

    deallocate(var)

    select type(var)
        type is (base)
        class default
            error stop
    end select

end program dealloc_05
