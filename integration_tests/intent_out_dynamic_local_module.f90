module intent_out_dynamic_local_m
    implicit none
    private
    public :: reset, check_local
    integer, parameter :: initial = 23
contains
    subroutine reset(x)
        class(*), intent(out) :: x
    end subroutine

    subroutine check_local()
        type :: item
            integer :: value = initial
            integer, allocatable :: data(:)
        end type
        type(item) :: x
        allocate(x%data(2))
        x%value = -1
        call reset(x)
        if (allocated(x%data) .or. x%value /= initial) error stop 1
        block
            integer, parameter :: offset = 4
            block
                type :: item
                    integer :: value = initial + offset
                    integer, allocatable :: data(:)
                    character(3), pointer :: text => null()
                end type
                type(item) :: y
                character(3), target :: target = 'abc'
                allocate(y%data(3))
                y%value = -1
                y%text => target
                call reset(y)
                if (allocated(y%data) .or. y%value /= initial + offset) error stop 2
                if (associated(y%text) .or. target /= 'abc') error stop 3
            end block
        end block
        block
            type :: item
                integer :: value = initial + 2
                integer, allocatable :: data(:)
            end type
            type(item) :: y
            allocate(y%data(4))
            y%value = -1
            call reset(y)
            if (allocated(y%data) .or. y%value /= initial + 2) error stop 4
        end block
    end subroutine
end module
