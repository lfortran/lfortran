! Templates are an experimental prototype of a proposed Fortran feature. They
! are rejected unless `--enable-experimental-feature templates` is passed, so
! this test is deliberately registered without that option.
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
