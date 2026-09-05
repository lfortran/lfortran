! A separately compiled forwarder loads the interface from a .mod file.
! Forwarding a CHARACTER dummy must preserve its descriptor and length.
module implicit_interface_56_mod
    implicit none
    interface
        function get_vara_text_53(text) result(status)
            character(len=*), intent(out) :: text
            integer :: status
        end function get_vara_text_53
    end interface
end module implicit_interface_56_mod

! The external definition uses the same descriptor as its explicit interface.
function get_vara_text_53(text) result(status)
    implicit none
    character(len=*), intent(out) :: text
    integer :: status
    status = len(text)
    text = repeat("x", len(text))
end function get_vara_text_53

program implicit_interface_56
    implicit none
    character(len=14) :: s
    integer :: r
    s = ""
    call forward_text_53(s, r)
    print *, "status =", r, " s = [", s, "]"
    if (r /= 14) error stop
    if (s /= repeat("x", 14)) error stop
    print *, "OK"
end program implicit_interface_56
