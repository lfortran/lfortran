module intent_out_inherited_alloc_cleanup_02_m
    use iso_c_binding, only: c_int
    implicit none
    type :: base
        integer, allocatable :: inherited(:)
    end type
    type, extends(base) :: middle
        integer, allocatable :: middle_data(:)
    end type
    type :: nested
        integer, allocatable :: embedded(:)
    end type
    type, extends(middle) :: child
        integer, allocatable :: own(:)
        type(nested) :: inner
    end type
    type :: host
        type(child), allocatable :: owned
    end type
    interface
        subroutine begin_count() bind(c)
        end subroutine
        integer(c_int) function end_count() bind(c)
        end function
    end interface
contains
    subroutine reset(h)
        type(host), intent(out) :: h
    end subroutine
end module

program intent_out_inherited_alloc_cleanup_02
    use intent_out_inherited_alloc_cleanup_02_m
    implicit none
    type(host) :: h
    integer :: i
    do i = 1, 2
        call begin_count()
        allocate(h%owned)
        allocate(h%owned%inherited(i + 1))
        allocate(h%owned%middle_data(2))
        allocate(h%owned%own(3))
        allocate(h%owned%inner%embedded(4))
        h%owned%inherited = 11
        h%owned%middle_data = 13
        h%owned%own = 17
        h%owned%inner%embedded = 19
        if (h%owned%inherited(1) /= 11 .or. &
                h%owned%middle_data(1) /= 13 .or. &
                h%owned%own(1) /= 17 .or. &
                h%owned%inner%embedded(1) /= 19) error stop 1
        call reset(h)
        if (allocated(h%owned)) error stop 2
        if (end_count() /= 0_c_int) error stop 3
    end do
end program
