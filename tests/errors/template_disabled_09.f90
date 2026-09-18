! Templates are an experimental prototype of a proposed Fortran feature. Every
! construct of the prototype is rejected unless
! `--enable-experimental-feature templates` is passed, and each one is checked
! where it is turned into ASR, so each construct needs a test of its own: the
! first one reached ends the compilation.
! This one checks a templated subroutine reference. The templated subroutine is
! deliberately not defined here, for the same reason as in
! `template_disabled_08.f90`.
program template_disabled_09
implicit none
integer :: a, b

a = 1
b = 2
call swap_generic{integer}(a, b)

end program
