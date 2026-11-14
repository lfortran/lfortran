module intent_out_implicit_saved_target_module
    implicit none
    integer, target :: target
    type :: module_item
        integer, pointer :: p => target
    end type
contains
    subroutine reset(x)
        class(*), intent(out) :: x
    end subroutine
end module

program intent_out_implicit_saved_target
    use intent_out_implicit_saved_target_module, only: reset, module_item, module_target => target
    implicit none
    integer, target :: target
    ! This unused type alone used to give its reset entry a host-frame argument.
    type :: unused_item
        integer, pointer :: p => target
    end type
    type(module_item), allocatable :: x
    integer, pointer :: alias, escaped

    target = 23
    module_target = 41
    alias => target
    call check_main(23)
    call check_main(24)
    if (alias /= 25) error stop 1
    if (.not. associated(alias, target)) error stop 2

    allocate(x)
    nullify(x%p)
    call reset(x)
    if (.not. associated(x%p, module_target)) error stop 3
    x%p = 42
    nullify(x%p)
    call reset(x)
    if (.not. associated(x%p, module_target)) error stop 4
    if (x%p /= 42) error stop 5
    deallocate(x)

    call initialized_local(escaped, 31)
    if (escaped /= 32) error stop 6
    escaped = 35
    call sibling(51)
    call initialized_local(escaped, 35)
    if (escaped /= 36) error stop 7
    call sibling(52)
    if (target /= 25 .or. module_target /= 42) error stop 8
contains
    subroutine check_main(expected)
        integer, intent(in) :: expected
        if (target /= expected) error stop 9
        target = expected + 1
    end subroutine

    subroutine initialized_local(out, expected)
        integer, pointer, intent(out) :: out
        integer, intent(in) :: expected
        integer, target :: target = 31
        type :: item
            integer, pointer :: p => target
        end type
        type(item), allocatable :: x
        allocate(x)
        nullify(x%p)
        call reset(x)
        if (.not. associated(x%p, target)) error stop 11
        if (x%p /= expected) error stop 12
        out => x%p
        x%p = expected + 1
        deallocate(x)
    end subroutine

    subroutine sibling(expected)
        integer, intent(in) :: expected
        integer, target :: target = 51
        type :: item
            integer, pointer :: p => target
        end type
        if (target /= expected) error stop 13
        target = target + 1
    end subroutine
end program
