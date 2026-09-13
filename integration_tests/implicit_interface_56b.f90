! Companion to implicit_interface_56: forward the CHARACTER descriptor using
! the explicit interface loaded from the module file.
subroutine forward_text_53(string, rcode)
    use implicit_interface_56_mod
    implicit none
    character(len=*), intent(inout) :: string
    integer, intent(out) :: rcode
    rcode = get_vara_text_53(string)
end subroutine forward_text_53
