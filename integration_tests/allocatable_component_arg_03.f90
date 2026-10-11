! A pointer or a polymorphic allocatable derived-type component passed to a
! dummy that the callee may define keeps the callee's changes, also when the
! procedure is reached through a procedure pointer.
module allocatable_component_arg_03_mod
    implicit none
    type :: item_t
        integer :: x = 0
    end type
    type, extends(item_t) :: sub_t
        integer :: y = 0
    end type
    type :: holder_t
        type(item_t), pointer :: pitem => null()
        class(item_t), allocatable :: citem
        type(item_t), allocatable :: titem
    end type
    abstract interface
        subroutine none_iface(item)
            import :: item_t
            type(item_t) :: item
        end subroutine
    end interface
contains
    subroutine c_none(item)
        type(item_t) :: item
        item%x = item%x + 1
    end subroutine

    subroutine c_inout(item)
        type(item_t), intent(inout) :: item
        item%x = item%x + 10
    end subroutine

    subroutine c_class(item)
        class(item_t) :: item
        item%x = item%x + 100
        select type (item)
        type is (sub_t)
            item%y = item%y + 1
        end select
    end subroutine

    integer function f_none(item)
        type(item_t) :: item
        item%x = item%x + 1000
        f_none = item%x
    end function
end module

program allocatable_component_arg_03
    use allocatable_component_arg_03_mod
    implicit none
    type(holder_t) :: h
    type(item_t), target :: tg
    procedure(none_iface), pointer :: pn => null()
    integer :: r

    h%pitem => tg
    call c_none(h%pitem)
    if (tg%x /= 1) error stop 1
    call c_inout(h%pitem)
    if (tg%x /= 11) error stop 2
    call c_class(h%pitem)
    if (tg%x /= 111) error stop 3
    r = f_none(h%pitem)
    if (r /= 1111) error stop 4
    if (tg%x /= 1111) error stop 5

    allocate(item_t :: h%citem)
    call c_none(h%citem)
    if (h%citem%x /= 1) error stop 6
    call c_inout(h%citem)
    if (h%citem%x /= 11) error stop 7
    call c_class(h%citem)
    if (h%citem%x /= 111) error stop 8
    r = f_none(h%citem)
    if (r /= 1111) error stop 9
    if (h%citem%x /= 1111) error stop 10

    deallocate(h%citem)
    allocate(sub_t :: h%citem)
    call c_class(h%citem)
    if (h%citem%x /= 100) error stop 11
    select type (c => h%citem)
    type is (sub_t)
        if (c%y /= 1) error stop 12
    class default
        error stop 13
    end select

    pn => c_none
    call pn(h%pitem)
    if (tg%x /= 1112) error stop 14
    allocate(h%titem)
    call pn(h%titem)
    if (h%titem%x /= 1) error stop 15
    print *, tg%x, h%citem%x, h%titem%x
end program
