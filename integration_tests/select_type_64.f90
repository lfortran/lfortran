program select_type_64
    ! A default-initialized component whose type has a recursive pointer
    ! component, used from an internal procedure.
    implicit none
    type node
        type(node), pointer :: next => null()
        integer :: val = 7
    end type
    type cfg
        type(node) :: data = node()
    end type
    type(node), target :: t
    call work()
    call build()
contains
    subroutine work()
        class(*), allocatable :: x
        allocate(cfg :: x)
        select type (x)
        type is (cfg)
            if (associated(x%data%next)) error stop 1
            if (x%data%val /= 7) error stop 2
            x%data%next => t
            if (.not. associated(x%data%next, t)) error stop 3
            x%data%next => null()
            if (associated(x%data%next)) error stop 4
        class default
            error stop 5
        end select
    end subroutine

    subroutine build()
        type(node) :: n
        n = node()
        if (associated(n%next)) error stop 6
        if (n%val /= 7) error stop 7
        t%val = 3
        n = node(t, 5)
        if (.not. associated(n%next, t)) error stop 8
        if (n%next%val /= 3) error stop 9
        print *, "ok"
    end subroutine
end program
