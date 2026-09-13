! Regression test: an EXTERNAL function declared before another EXTERNAL
! function in a module specification part must keep its own implicit
! interface. Previously only the last-declared EXTERNAL survived, so calling
! any earlier one (here `cf`) wrongly reported
! "More actual than formal arguments in procedure call".
! Reduced from netcdf-fortran (fortran/netcdf4.F90).

function cf(k)
  integer :: k
  character(len=1) :: cf
  cf = char(iachar('a') + k)
end function cf

integer function nf(k)
  integer :: k
  nf = k + 1
end function nf

module external_17_mod
  implicit none
  character(len=1), external :: cf
  integer, external :: nf
contains
  function cs(k) result(res)
    integer :: k
    character(len=1) :: res
    res = cf(k)
  end function cs

  integer function ni(k)
    integer :: k
    ni = nf(k)
  end function ni
end module external_17_mod

program external_17
  use external_17_mod, only: cs, ni
  implicit none
  if (cs(2) /= 'c') error stop
  if (ni(4) /= 5) error stop
  print *, cs(2), ni(4)
end program external_17
