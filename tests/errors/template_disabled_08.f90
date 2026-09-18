! Templates are an experimental prototype of a proposed Fortran feature. Every
! construct of the prototype is rejected unless
! `--enable-experimental-feature templates` is passed, and each one is checked
! where it is turned into ASR, so each construct needs a test of its own: the
! first one reached ends the compilation.
! This one checks a templated function reference. The templated function is
! deliberately not defined here: the reference is rejected before the name is
! resolved, and defining it would be rejected at the definition instead.
program template_disabled_08
implicit none
integer :: s

s = add_generic{integer}(1, 2)
print *, s

end program
