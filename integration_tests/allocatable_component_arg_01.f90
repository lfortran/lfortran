! An allocatable scalar derived-type component passed to a dummy that the
! callee may define keeps the callee's changes.
module allocatable_component_arg_01_mod
    implicit none
    type :: item_t
        integer :: x = 0
    end type
    type :: holder_t
        type(item_t), allocatable :: titem
    end type
    type :: outer_t
        type(holder_t) :: h
    end type
contains
    subroutine c_none(item)
        type(item_t) :: item
        item%x = item%x + 1
    end subroutine

    subroutine c_inout(item)
        type(item_t), intent(inout) :: item
        item%x = item%x + 10
    end subroutine

    subroutine c_out(item)
        type(item_t), intent(out) :: item
        item%x = 100
    end subroutine

    integer function f_in(item)
        type(item_t), intent(in) :: item
        f_in = item%x
    end function

    subroutine c_class(item)
        class(item_t) :: item
        item%x = item%x + 2
    end subroutine

    subroutine c_alloc(item)
        type(item_t), allocatable :: item
        item%x = item%x + 3
    end subroutine

    integer function f_none(item)
        type(item_t) :: item
        item%x = item%x + 5
        f_none = item%x
    end function

    logical function is_present(item)
        type(item_t), optional :: item
        is_present = present(item)
    end function
end module

program allocatable_component_arg_01
    use allocatable_component_arg_01_mod
    implicit none
    type(holder_t) :: h, hs(3)
    type(outer_t) :: o
    integer :: r

    if (is_present(h%titem)) error stop
    allocate(h%titem)
    if (.not. is_present(h%titem)) error stop

    call c_none(h%titem)
    if (h%titem%x /= 1) error stop
    call c_inout(h%titem)
    if (h%titem%x /= 11) error stop
    call c_out(h%titem)
    if (h%titem%x /= 100) error stop
    if (f_in(h%titem) /= 100) error stop
    call c_class(h%titem)
    if (h%titem%x /= 102) error stop
    call c_alloc(h%titem)
    if (h%titem%x /= 105) error stop
    r = f_none(h%titem)
    if (r /= 110) error stop
    if (h%titem%x /= 110) error stop

    allocate(hs(2)%titem)
    call c_none(hs(2)%titem)
    if (hs(2)%titem%x /= 1) error stop

    allocate(o%h%titem)
    call c_none(o%h%titem)
    call c_none(o%h%titem)
    if (o%h%titem%x /= 2) error stop
    print *, h%titem%x, hs(2)%titem%x, o%h%titem%x
end program
