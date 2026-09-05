! A module declares a procedure `external` with an implicit interface and the
! procedure is referenced with NO actual arguments from a separate program that
! use-associates the module. Inference of the concrete interface at the
! reference was gated on the reference having at least one argument, so a
! zero-argument reference was left naming the bare declaration, which is not a
! call target, and ASR verification rejected it with "was declared external
! with no interface". Standard Fortran allows the reference (flang and gfortran
! accept it). This mirrors netcdf-fortran's `nf_inq_libvers()`, which takes no
! arguments and is declared `external` in a module.
program implicit_interface_49
  use implicit_interface_49_mod
  implicit none
  integer :: r
  r = nf_inq_format()
  print *, "r =", r
  if (r /= 7) error stop
end program implicit_interface_49
