program scalar_allocation_check_02
    implicit none

    type :: base
        integer :: x
    end type

    class(base), allocatable :: var

    

    select type(var)
        type is (base)
            print *, var%x
        class default
            error stop
    end select

end program
