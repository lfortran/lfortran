program procedure_pointer_abstract_02
implicit none

abstract interface
    subroutine callback_iface(c)
        character(len=*), intent(in) :: c
    end subroutine
end interface

call runner("hello", gen_check)

contains

subroutine runner(chr, checker)
    character(len=*), intent(in) :: chr
    procedure(callback_iface) :: checker
    call checker(chr)
end subroutine

subroutine gen_check(c)
    character(len=*), intent(in) :: c
    if (len(c) /= 5) error stop
    if (c /= "hello") error stop
end subroutine

end program
