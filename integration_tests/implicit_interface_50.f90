! The character-valued form of implicit_interface_49: a zero-argument reference
! to a `character(len=*), external` declared in a use-associated module. Because
! a character-returning function is rewritten into a subroutine, failing to
! infer the interface at the reference left a SubroutineCall naming the bare
! declaration, which failed in the subroutine_from_function pass rather than at
! the call site. This mirrors netcdf-fortran's `nf_inq_libvers()`.
program implicit_interface_50
  use implicit_interface_50_mod
  implicit none
  character(len=80) :: v
  v = nf_inq_libvers()
  print *, trim(v)
  if (trim(v) /= "netcdf-fortran 1.2.3") error stop
end program implicit_interface_50
