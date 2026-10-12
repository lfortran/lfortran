module allocatable_component_arg_05_mod
    implicit none
    integer :: final_count = 0
    type :: payload_t
        integer :: v = 1
    contains
        final :: finish
    end type
    type :: parent_t
        type(payload_t), allocatable :: child
        type(payload_t), pointer :: pchild => null()
    end type
contains
    subroutine finish(x)
        type(payload_t) :: x
        final_count = final_count + 1
    end subroutine

    subroutine consume(x)
        type(payload_t) :: x
    end subroutine

    subroutine consume_in(x)
        type(payload_t), intent(in) :: x
        if (x%v /= 1) error stop
    end subroutine

    subroutine consume_inout(x)
        type(payload_t), intent(inout) :: x
        x%v = x%v + 1
    end subroutine

    subroutine consume_class(x)
        class(payload_t), intent(in) :: x
        if (x%v /= 2) error stop
    end subroutine

    integer function get_v(x)
        type(payload_t), intent(in) :: x
        get_v = x%v
    end function
end module

program allocatable_component_arg_05
    use allocatable_component_arg_05_mod
    implicit none
    type(parent_t) :: parent
    type(parent_t), allocatable :: aparent
    integer :: k

    allocate(parent%child)
    call consume(parent%child)
    print *, final_count
    if (final_count /= 0) error stop

    call consume_in(parent%child)
    if (final_count /= 0) error stop

    call consume_inout(parent%child)
    if (final_count /= 0) error stop
    if (parent%child%v /= 2) error stop

    call consume_class(parent%child)
    if (final_count /= 0) error stop

    k = get_v(parent%child)
    if (k /= 2) error stop
    if (final_count /= 0) error stop

    allocate(parent%pchild)
    call consume(parent%pchild)
    call consume_in(parent%pchild)
    call consume_inout(parent%pchild)
    call consume_class(parent%pchild)
    k = get_v(parent%pchild)
    if (k /= 2) error stop
    if (final_count /= 0) error stop

    allocate(aparent)
    allocate(aparent%child)
    call consume(aparent%child)
    call consume_inout(aparent%child)
    call consume_class(aparent%child)
    if (final_count /= 0) error stop

    if (.not. allocated(parent%child)) error stop
    if (.not. allocated(aparent%child)) error stop
    deallocate(parent%pchild)
    if (final_count /= 1) error stop
    print *, final_count
end program
