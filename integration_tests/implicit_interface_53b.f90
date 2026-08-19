! Separately-compiled forwarder, mirroring netcdf-fortran's ncvgtc. It receives
! its own assumed-length CHARACTER(len=*) dummy and forwards it to the external
! function get_vara_text_53, whose explicit interface it obtains from the USEd
! module implicit_interface_53_mod. Because this file is compiled in its own
! translation unit (under --separate-compilation), the module and its interface
! body are loaded from a .mod file; the forwarded hidden character length must
! still reach get_vara_text_53 so that LEN(text) inside it is 14, not garbage.
subroutine forward_text_53(string, rcode)
    use implicit_interface_53_mod
    implicit none
    character(len=*), intent(inout) :: string
    integer, intent(out) :: rcode
    rcode = get_vara_text_53(string)
end subroutine forward_text_53
