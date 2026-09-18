! Templates are an experimental prototype of a proposed Fortran feature and are
! rejected unless `--enable-experimental-feature templates` is passed. Under
! `--continue-compilation` each rejected construct must be reported exactly
! once (it is checked while the ASR is built, and both the symbol table and the
! body visitor reach some of them), and compilation must carry on and still
! report the ordinary errors that follow.
module continue_compilation_templates_disabled_01_mod
implicit none

template add_t(t)
    deferred type :: t
end template

contains

    integer function f(x)
    require :: add_r(integer)
    integer, intent(in) :: x
    f = x + 1
    end function

    integer function g(x)
    integer, intent(in) :: x
    type(nosuchtype) :: v
    g = x
    end function

end module

program continue_compilation_templates_disabled_01
use continue_compilation_templates_disabled_01_mod
implicit none
end program
