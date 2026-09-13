! A module declares a procedure `external` with an implicit interface (no
! argument information). When that module is compiled to a .mod file and
! use-associated in a separate program that calls the procedure with actual
! arguments, LFortran previously rejected the call with "More actual than
! formal arguments in procedure call": the external, read back from the .mod
! file, was recorded with zero formal arguments and its implicit-interface
! nature was lost. Standard Fortran performs no argument checking for
! implicit-interface externals, so the call is valid (flang and gfortran accept
! it). This mirrors netcdf-fortran's module_netcdf4_nf_interfaces.F90, which
! declares `Integer, External :: nf_def_var_fill`, called from
! nf_test4/f03tst_open_mem.F.
program implicit_interface_47
  use implicit_interface_47_mod
  implicit none
  integer :: ncid, varid(3), no_fill, retval
  ncid = 1
  varid(2) = 2
  no_fill = 1
  retval = nf_def_var_fill(ncid, varid(2), no_fill, 88)
  print *, "retval =", retval
  if (retval /= 92) error stop
end program implicit_interface_47
