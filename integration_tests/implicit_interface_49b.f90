! Separately compiled external function with a CHARACTER(len=*) dummy,
! mirroring netcdf-fortran's nf_put_att_text (fortran/nf_attio.F90). Through an
! implicit interface the caller must pass the character actual by the classic
! Fortran hidden-length ABI (data pointer + hidden trailing length). Before the
! fix, an allocatable/deferred-length actual such as trim(...) made LFortran
! synthesize an allocatable dummy and pass a { data, len } string descriptor
! with a missing hidden length, so the callee saw a bogus length and garbage
! data.
integer function check_text(n, text) result(status)
  implicit none
  integer, intent(in) :: n
  character(len=*), intent(in) :: text
  status = 0
  if (len(text) /= n) status = status + 1
  if (text /= "hours") status = status + 2
end function check_text
