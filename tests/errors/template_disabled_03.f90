! Templates are an experimental prototype of a proposed Fortran feature. Every
! construct of the prototype is rejected unless
! `--enable-experimental-feature templates` is passed, and each one is checked
! where it is turned into ASR, so each construct needs a test of its own: the
! first one reached ends the compilation.
! This one checks the `require` statement.
module template_disabled_03_m
implicit none

contains

    integer function f(x)
    require :: add_r(integer)
    integer, intent(in) :: x
    f = x + 1
    end function

end module
