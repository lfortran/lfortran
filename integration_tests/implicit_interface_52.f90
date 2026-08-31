! Passing trim(...) -- an allocatable, deferred-length CHARACTER result -- to a
! CHARACTER(len=*) dummy of a separately compiled external procedure reached
! through an implicit interface. This mirrors netcdf-fortran's nf90_put_att_text
! (fortran/netcdf_attributes.F90) calling the external nf_put_att_text as
!     nf_put_att_text(ncid, varid, name, len_trim(values), trim(values))
! LFortran synthesized an *allocatable* dummy for the implicit interface and
! passed a { data, len } string descriptor plus a missing hidden length, so the
! callee received a corrupted string (a bogus length and pointer bytes as data).
! The classic Fortran hidden-length ABI passes the data pointer directly and the
! length as a hidden trailing argument (matching gfortran/flang).
module implicit_interface_52_mod
  implicit none
contains
  integer function put_text(values) result(status)
    character(len=*), intent(in) :: values
    integer, external :: check_text
    status = check_text(len_trim(values), trim(values))
  end function put_text
end module implicit_interface_52_mod

program implicit_interface_52
  use implicit_interface_52_mod
  implicit none
  integer :: status
  status = put_text("hours   ")
  print *, "status =", status
  if (status /= 0) error stop
  print *, "OK"
end program implicit_interface_52
