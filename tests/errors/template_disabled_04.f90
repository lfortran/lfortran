! Templates are an experimental prototype of a proposed Fortran feature. Every
! construct of the prototype is rejected unless
! `--enable-experimental-feature templates` is passed, and each one is checked
! where it is turned into ASR, so each construct needs a test of its own: the
! first one reached ends the compilation.
! This one checks the `instantiate` statement.
program template_disabled_04
implicit none

instantiate add_t(integer), only: add_integer => add_generic

end program
