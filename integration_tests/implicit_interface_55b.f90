! Companion to implicit_interface_55: return the length from the descriptor.
function nf_get_vara_text(text) result(status)
    implicit none
    character(len=*), intent(out) :: text
    integer :: status
    status = len(text)
end function nf_get_vara_text
