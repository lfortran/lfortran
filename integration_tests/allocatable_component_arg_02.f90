! An allocatable or pointer derived-type component passed to a VALUE dummy
! is copied: the callee's changes do not reach the component.
module allocatable_component_arg_02_mod
    use iso_c_binding, only: c_int
    implicit none
    type, bind(c) :: citem_t
        integer(c_int) :: v = 0
    end type
    type :: item_t
        integer :: v = 0
    end type
    type :: holder_t
        type(citem_t), allocatable :: cit
        type(item_t), allocatable :: it
        type(item_t), pointer :: p => null()
    end type
contains
    subroutine c_val(x) bind(c)
        type(citem_t), value :: x
        x%v = 77
    end subroutine

    integer(c_int) function c_val_f(x) bind(c)
        type(citem_t), value :: x
        x%v = x%v + 1
        c_val_f = x%v
    end function

    subroutine f_val(x)
        type(item_t), value :: x
        x%v = 77
    end subroutine
end module

program allocatable_component_arg_02
    use allocatable_component_arg_02_mod
    implicit none
    type(holder_t) :: h
    type(item_t), target :: tg

    allocate(h%cit)
    h%cit%v = 4
    call c_val(h%cit)
    if (h%cit%v /= 4) error stop 1
    if (c_val_f(h%cit) /= 5) error stop 2
    if (h%cit%v /= 4) error stop 3

    allocate(h%it)
    h%it%v = 4
    call f_val(h%it)
    if (h%it%v /= 4) error stop 4

    tg%v = 4
    h%p => tg
    call f_val(h%p)
    if (tg%v /= 4) error stop 5
    if (h%p%v /= 4) error stop 6
    print *, "ok"
end program
