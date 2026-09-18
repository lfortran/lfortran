! Templates are an experimental prototype of a proposed Fortran feature. Every
! construct of the prototype is rejected unless
! `--enable-experimental-feature templates` is passed, and each one is checked
! where it is turned into ASR, so each construct needs a test of its own: the
! first one reached ends the compilation.
! This one checks a templated function definition.
module template_disabled_06_m
implicit none

contains

    function add_generic{t}(x, y) result(z)
    integer, intent(in) :: x, y
    integer :: z
    z = x + y
    end function

end module
