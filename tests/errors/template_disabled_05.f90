! Templates are an experimental prototype of a proposed Fortran feature. Every
! construct of the prototype is rejected unless
! `--enable-experimental-feature templates` is passed, and each one is checked
! where it is turned into ASR, so each construct needs a test of its own: the
! first one reached ends the compilation.
! This one checks the `deferred type ::` declaration. The `deferred` attribute
! of a type bound procedure is standard Fortran and is not affected.
module template_disabled_05_m
implicit none

deferred type :: t

end module
