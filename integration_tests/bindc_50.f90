module bindc_50_mod
  use iso_c_binding, only: c_int
  implicit none
  ! bind(c, name='') on a module variable: per F2018 18.3.7.1 the
  ! binding label is empty, so no C binding is established. This
  ! exercises the LLVM codegen branch that falls back to the mangled
  ! Fortran name.
  integer(c_int), bind(c, name='') :: v_empty
  integer(c_int), bind(c)          :: v_noname
  integer(c_int), bind(c, name='v_explicit') :: v_named
end module

program bindc_50
  use bindc_50_mod
  implicit none
  v_empty = 11
  v_noname = 22
  v_named = 33
  if (v_empty /= 11) error stop
  if (v_noname /= 22) error stop
  if (v_named /= 33) error stop
  print *, v_empty, v_noname, v_named
end program
