! A template procedure with a host-module variable as an explicit-shape
! bound, instantiated in a module and used from another file: the
! instantiation written to the module file must not refer to the template's
! getter for the bound.
module template_host_var_07_m
    implicit none
    integer :: n = 3
    template tmpl {t}
        deferred type :: t
    contains
        subroutine fill(a, x)
            type(t), intent(inout) :: a(n)
            type(t), intent(in) :: x
            a = x
        end subroutine
    end template
end module

module template_host_var_07_user
    use template_host_var_07_m, only: tmpl, n
    implicit none
    instantiate tmpl {integer}, only: ifill => fill
    instantiate tmpl {real}, only: rfill => fill
end module
