! A separately compiled external procedure (forward_text_53, in
! implicit_interface_56b.f90) forwards its own assumed-length CHARACTER(len=*)
! dummy to another external procedure (get_vara_text_53) through an interface
! block declared in a MODULE. This is the netcdf-fortran v2 idiom: ncvgtc
! (nf_fortv2.F90) forwards its CHARACTER(len=*) argument to nf_get_vara_text
! (nf_varaio.F90) via the interface in module_netcdf_nf_interfaces.
!
! Under --separate-compilation the forwarder loads the module from a .mod file,
! which rewrites the interface body's ABI from Source to ExternalUndefined.
! LFortran previously only recognized the Source form as a plain external
! interface block, so the ExternalUndefined form was miscompiled with the
! string-descriptor ABI while the external definition used the classic
! (data pointer + hidden trailing length) ABI. The hidden length was never
! delivered and LEN(text) inside the callee came out as garbage, so
! text = repeat(...) attempted a huge copy and crashed with a bus error.
module implicit_interface_56_mod
    implicit none
    interface
        function get_vara_text_53(text) result(status)
            character(len=*), intent(out) :: text
            integer :: status
        end function get_vara_text_53
    end interface
end module implicit_interface_56_mod

! External definition: classic hidden-length CHARACTER ABI. LEN(text) must be
! the caller's length (14), not garbage.
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
