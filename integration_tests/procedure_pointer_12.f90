module procedure_pointer_12_mod
    implicit none

    abstract interface
        subroutine callback_iface(flag, code_int, code_char)
            logical, intent(in) :: flag
            integer, intent(in), optional :: code_int
            character(len=*), intent(in), optional :: code_char
        end subroutine
    end interface

    type :: callback_entry
        procedure(callback_iface), pointer, nopass :: callback => null()
    end type

contains

    subroutine my_callback(flag, code_int, code_char)
        logical, intent(in) :: flag
        integer, intent(in), optional :: code_int
        character(len=*), intent(in), optional :: code_char
        if (present(code_int)) then
            if (code_int /= 42) error stop
        end if
        if (present(code_char)) then
            if (code_char /= "hello") error stop
        end if
    end subroutine

    subroutine run_callback(entry, flag, code_int, code_char)
        type(callback_entry), intent(in) :: entry
        logical, intent(in) :: flag
        integer, intent(in), optional :: code_int
        character(len=*), intent(in), optional :: code_char
        call entry%callback(flag, code_int, code_char)
    end subroutine

end module

program procedure_pointer_12
    use procedure_pointer_12_mod
    implicit none
    type(callback_entry) :: e
    e%callback => my_callback
    call run_callback(e, .true., code_int=42)
    call run_callback(e, .true., code_char="hello")
    call run_callback(e, .true.)
    print *, "ok"
end program
