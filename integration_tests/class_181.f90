module class_181_m
    implicit none
    integer, target :: default_target = 17
    type :: base
        integer :: root = -1
        integer, allocatable :: values(:)
    end type
contains
    subroutine reset(x)
        class(base), intent(out) :: x
    end subroutine
end module

program class_181
    use class_181_m
    implicit none
    block
        integer, target :: alternative = 6
        type, extends(base) :: child
            integer :: mark = 42
            integer, allocatable :: nested(:)
            integer, pointer :: ref => default_target
        end type
        type(child) :: x
        x%root = 9
        x%mark = 17
        allocate(x%values(1), x%nested(2))
        x%ref => alternative
        call reset(x)
        if (x%root /= -1) error stop 1
        if (x%mark /= 42) error stop 2
        if (allocated(x%values)) error stop 3
        if (allocated(x%nested)) error stop 4
        if (.not. associated(x%ref, default_target)) error stop 10
        if (alternative /= 6) error stop 11
    end block
    block
        type, extends(base) :: child
            integer :: mark = 84
        end type
        type(child) :: x
        x%root = 9
        x%mark = 17
        call reset(x)
        if (x%root /= -1) error stop 5
        if (x%mark /= 84) error stop 6
    end block
    call check_nested()
    associate (initial => default_target)
        block
            type, extends(base) :: child
                integer :: mark = 168
            end type
            type(child) :: x
            x%mark = initial
            call reset(x)
            if (x%root /= -1) error stop 12
            if (x%mark /= 168) error stop 13
        end block
    end associate
contains
    subroutine check_nested()
        block
            block
                type, extends(base) :: child
                    integer :: mark = 126
                    integer, allocatable :: nested(:)
                end type
                type(child) :: x
                x%root = 9
                x%mark = 17
                allocate(x%nested(1))
                call reset(x)
                if (x%root /= -1) error stop 7
                if (x%mark /= 126) error stop 8
                if (allocated(x%nested)) error stop 9
            end block
        end block
    end subroutine
end program
