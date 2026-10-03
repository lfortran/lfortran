! Coarray runtime interfaces synthesized in global scope must preserve the
! CHARACTER descriptor expected by the corresponding Fortran module procedures.
program coarray_prif_char_abi_01
  implicit none
  integer :: n
  n = 1
  sync all
  print *, n
end program coarray_prif_char_abi_01
