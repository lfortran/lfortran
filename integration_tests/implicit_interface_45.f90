! A typed external function (implicit interface) called from a program that
! also has a CONTAINS section with an internal procedure must still be treated
! as having the arguments passed at the call site. LFortran previously wiped
! the host scope's list of external procedures while processing the contained
! subroutine, so the later call `nf_create(ncid)` was wrongly rejected with
! "More actual than formal arguments in procedure call". This mirrors
! netcdf-fortran's ftst_path.F, where nf_* externals are declared via
! netcdf.inc in a program that also CONTAINS a `check` subroutine.
program implicit_interface_45
  implicit none
  integer :: nf_create
  external :: nf_create
  integer :: ncid, ierr
  ncid = 41
  ierr = nf_create(ncid)
  call check(ierr)
  if (ierr /= 42) error stop
  print *, "ierr =", ierr
contains
  subroutine check(errcode)
    integer, intent(in) :: errcode
    if (errcode /= 42) error stop
  end subroutine check
end program implicit_interface_45

integer function nf_create(x)
  implicit none
  integer, intent(in) :: x
  nf_create = x + 1
end function nf_create
