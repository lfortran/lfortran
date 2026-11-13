module c_pointer_descriptor_ownership_02_m
    use iso_c_binding, only: c_int
    implicit none
    type :: base
        integer, pointer :: inherited(:) => null()
    end type
    type, extends(base) :: child
        integer, pointer :: direct(:) => null()
    end type
    type :: box
        type(child) :: nested
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
    subroutine retarget(p, target)
        integer, pointer, intent(inout) :: p(:)
        integer, target, intent(in) :: target(-2:)
        p => target
    end subroutine

    subroutine exercise()
        type(box), allocatable :: first, second
        integer, target :: values(-2:2), alternate(3)
        values(-2) = 13
        values(2) = 19
        alternate(3) = 23
        allocate(first, second)
        if (associated(first%nested%inherited)) error stop 1
        call retarget(first%nested%inherited, values)
        if (.not. associated(first%nested%inherited, values)) error stop 10
        if (lbound(first%nested%inherited, 1) /= -2) error stop 11
        second%nested%inherited => values
        first%nested%direct => second%nested%inherited
        if (.not. associated(first%nested%direct, values)) error stop 2
        first%nested%inherited => alternate
        if (first%nested%inherited(3) /= 23) error stop 3
        second%nested%inherited => null()
        if (associated(second%nested%inherited)) error stop 4
        deallocate(second)
        if (first%nested%direct(-2) /= 13) error stop 5
        if (first%nested%direct(2) /= 19) error stop 6
        deallocate(first)
        if (allocated(first)) error stop 7
        if (values(-2) /= 13) error stop 8
        if (values(2) /= 19) error stop 12
    end subroutine
end module

program c_pointer_descriptor_ownership_02
    use c_pointer_descriptor_ownership_02_m
    implicit none
    integer :: i
    do i = 1, 2
        call begin_count()
        call exercise()
        if (end_count() /= 0_c_int) error stop 9
    end do
end program
