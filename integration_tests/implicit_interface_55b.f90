! Separately-compiled external function with an assumed-length CHARACTER(len=*)
! dummy, mirroring netcdf-fortran's nf_get_vara_text. It is declared through an
! interface block inside a module in implicit_interface_55.f90. The classic
! Fortran ABI passes the character data pointer at the argument position and the
! length as a hidden trailing argument; the module interface block must agree so
! that LEN(text) inside the callee is the caller's length (14), not garbage.
function nf_get_vara_text(text) result(status)
    implicit none
    character(len=*), intent(out) :: text
    integer :: status
    status = len(text)
end function nf_get_vara_text
