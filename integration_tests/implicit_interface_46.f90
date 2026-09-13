! A module declares an external procedure with an implicit interface (no
! argument information) and that module is use-associated inside a CONTAINS'd
! subroutine of a program that ALSO calls the same external by name with actual
! arguments. Both the module's zero-argument view and the program's call site
! refer to the same link-time symbol (nf_create) and are emitted with C
! linkage. LFortran previously let the module's zero-argument declaration win,
! so the host call `nf_create(1, 2, 3)` was lowered against
! `declare i32 @nf_create()` and the LLVM verifier rejected the module with
! "Incorrect number of arguments passed to called function!". This mirrors
! netcdf-fortran's ftst_path.F, where nf_* externals are declared via
! netcdf.inc in a program that also CONTAINS a `check` subroutine doing
! `use netcdf`.
program implicit_interface_46
  implicit none
  integer, external :: nf_create
  integer :: r
  r = nf_create(1, 2, 3)
  print *, r
  if (r /= 6) error stop
contains
  subroutine check()
    use implicit_interface_46_mod
  end subroutine check
end program implicit_interface_46
