! Templates are an experimental prototype of a proposed Fortran feature. Every
! construct of the prototype is rejected unless
! `--enable-experimental-feature templates` is passed, and each one is checked
! where it is turned into ASR, so each construct needs a test of its own: the
! first one reached ends the compilation.
! This one checks the `requirement` construct.
module template_disabled_02_m
implicit none

requirement add_r(t)
    deferred type :: t
end requirement

end module
