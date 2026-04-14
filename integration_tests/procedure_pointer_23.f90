module procedure_pointer_23_mod
    implicit none
    abstract interface
        subroutine cb_iface(x, y)
            integer, intent(in) :: x
            character(len=*), optional, intent(in) :: y
        end subroutine
    end interface
    type :: t
        procedure(cb_iface), nopass, pointer :: cb => null()
    end type
    integer :: g_x = 0
contains
    subroutine call_it(obj)
        type(t), intent(in) :: obj
        if (associated(obj%cb)) call obj%cb(1, "hi")
    end subroutine

    subroutine my_cb(x, y)
        integer, intent(in) :: x
        character(len=*), optional, intent(in) :: y
        g_x = x
        if (present(y)) then
            if (len(y) /= 2) error stop
            if (y /= "hi") error stop
        end if
    end subroutine
end module

program procedure_pointer_23
    use procedure_pointer_23_mod
    implicit none
    type(t) :: obj

    obj%cb => my_cb
    call call_it(obj)
    if (g_x /= 1) error stop
    print *, "PASSED"
end program
