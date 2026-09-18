! Templates are an experimental prototype of a proposed Fortran feature. Every
! construct of the prototype is rejected unless
! `--enable-experimental-feature templates` is passed, and each one is checked
! where it is turned into ASR, so each construct needs a test of its own: the
! first one reached ends the compilation.
! This one checks the `template` construct.
module template_disabled_01_m
implicit none

template add_t(t)
    deferred type :: t
contains
    function add_generic(x, y) result(z)
        type(t), intent(in) :: x, y
        type(t) :: z
        z = x + y
    end function
end template

end module
