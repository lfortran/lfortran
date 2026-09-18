! Templates are an experimental prototype of a proposed Fortran feature. Every
! construct of the prototype is rejected unless
! `--enable-experimental-feature templates` is passed, and each one is checked
! where it is turned into ASR, so each construct needs a test of its own: the
! first one reached ends the compilation.
! This one checks a templated subroutine definition.
module template_disabled_07_m
implicit none

contains

    subroutine swap_generic{t}(x, y)
    integer, intent(inout) :: x, y
    x = y
    end subroutine

end module
