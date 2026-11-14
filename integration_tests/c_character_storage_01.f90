module c_character_storage_01_m
    use iso_c_binding, only: c_int
    implicit none
    type :: base
        character(len=4) :: inherited
    end type
    type :: nested
        character(len=4) :: text
    end type
    type, extends(base) :: item
        character(len=4) :: value
        type(nested) :: inner
    end type
    interface
        subroutine begin_count() bind(c)
        end subroutine
        function end_count() result(n) bind(c)
            import :: c_int
            integer(c_int) :: n
        end function
    end interface
contains
    subroutine exercise()
        type(item), allocatable :: x
        character(len=4) :: local
        call begin_count()
        allocate(x)
        local = 'a'
        local = local
        if (local /= 'a   ') error stop 1
        x%inherited = 'ab'
        x%inner%text = 'cd'
        x%value = 'a'
        x%value = 'abcdef'
        x%value = x%value
        if (x%inherited /= 'ab  ' .or. x%inner%text /= 'cd  ' .or. &
                x%value /= 'abcd') error stop 2
        deallocate(x)
        if (allocated(x)) error stop 3
        if (end_count() /= 0_c_int) error stop 4
    end subroutine
end module

program c_character_storage_01
    use c_character_storage_01_m
    implicit none
    call exercise()
    call exercise()
end program
