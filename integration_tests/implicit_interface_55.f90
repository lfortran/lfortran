! An external procedure with an assumed-length CHARACTER(len=*) dummy is
! declared through an interface block inside a MODULE, but is DEFINED in a
! separate compilation unit (implicit_interface_55b.f90). This is the
! netcdf-fortran idiom (module_netcdf_nf_interfaces declares nf_get_vara_text,
! defined in nf_varaio.F90).
!
! LFortran previously gave the module interface-block call site the
! string-descriptor ABI (a single %string_descriptor*) while the separately
! compiled top-level definition used the classic (data pointer + hidden length)
! ABI. The hidden length was therefore never delivered and LEN(text) inside the
! callee was garbage (netcdf crashed with a bus error). A module-owned interface
! body that is not used as a procedure interface (i.e. not an abstract
! interface) now uses the classic ABI, matching the external definition.
module netcdf_nf_interfaces_52
    interface
        function nf_get_vara_text(text) result(status)
            character(len=*), intent(out) :: text
            integer :: status
        end function nf_get_vara_text
    end interface
end module netcdf_nf_interfaces_52

program implicit_interface_55
    use netcdf_nf_interfaces_52
    implicit none
    character(len=14) :: s
    integer :: r
    r = nf_get_vara_text(s)
    print *, "LEN(text) seen by callee =", r, " (expected 14)"
    if (r /= 14) error stop
    print *, "OK"
end program implicit_interface_55
