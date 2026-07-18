! Separately compiled external INTEGER FUNCTION with a scalar CHARACTER(len=*),
! intent(out) dummy, mirroring netcdf-fortran's nf_get_var_text
! (fortran/nf_vario.F90). Through an implicit interface the caller passes the
! character actual by the classic Fortran hidden-length ABI (data pointer +
! hidden trailing per-element length). Returning LEN(text) exposes the length
! the caller passed; `text = ' '` is the blanking assignment that crashed with a
! garbage length before the ABI fix.
integer function get_text(text) result(n)
    implicit none
    character(len=*), intent(out) :: text
    n = len(text)
    text = ' '
end function get_text
