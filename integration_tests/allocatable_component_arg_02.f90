! An allocatable or pointer derived-type component passed to a VALUE dummy
! is copied: the callee's changes do not reach the component, also when the
! procedure is reached through a procedure pointer or a dummy procedure.
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
        type(citem_t), pointer :: cp => null()
    end type
    abstract interface
        subroutine c_iface(x) bind(c)
            import :: citem_t
            type(citem_t), value :: x
        end subroutine

        integer(c_int) function c_get_iface(x) bind(c)
            import :: citem_t, c_int
            type(citem_t), value :: x
        end function

        subroutine f_iface(x)
            import :: item_t
            type(item_t), value :: x
        end subroutine
    end interface
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

    integer(c_int) function c_get(x) bind(c)
        type(citem_t), value :: x
        c_get = x%v
    end function

    subroutine f_val(x)
        type(item_t), value :: x
        x%v = 77
    end subroutine

    subroutine call_c(f, h)
        procedure(c_iface) :: f
        type(holder_t), intent(inout) :: h
        call f(h%cit)
    end subroutine

    integer function call_c_get(f, h)
        procedure(c_get_iface) :: f
        type(holder_t), intent(in) :: h
        call_c_get = f(h%cp)
    end function

    subroutine call_f(f, h)
        procedure(f_iface) :: f
        type(holder_t), intent(inout) :: h
        call f(h%it)
        call f(h%p)
    end subroutine
end module

program allocatable_component_arg_02
    use allocatable_component_arg_02_mod
    implicit none
    type(holder_t) :: h
    type(item_t), target :: tg
    type(citem_t), target :: ctg
    procedure(c_iface), pointer :: pc => null()
    procedure(c_get_iface), pointer :: pg => null()
    procedure(f_iface), pointer :: pf => null()

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

    ! The same through a procedure pointer and through a dummy procedure.
    pc => c_val
    call pc(h%cit)
    if (h%cit%v /= 4) error stop 7
    call call_c(c_val, h)
    if (h%cit%v /= 4) error stop 8

    ctg%v = 4
    h%cp => ctg
    pg => c_get
    if (pg(h%cp) /= 4) error stop 9
    if (call_c_get(c_get, h) /= 4) error stop 10

    pf => f_val
    call pf(h%it)
    if (h%it%v /= 4) error stop 11
    call pf(h%p)
    if (tg%v /= 4) error stop 12
    call call_f(f_val, h)
    if (h%it%v /= 4) error stop 13
    if (tg%v /= 4) error stop 14
    print *, "ok"
end program
