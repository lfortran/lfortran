! A template procedure with a host-module bound on a dummy and on a local,
! instantiated only because another template procedure calls it: its
! dependencies must include the instantiated getter of the bound.
module template_host_var_06_m
    implicit none
    integer :: n = 3
    template tmpl {t}
        deferred type :: t
    contains
        subroutine fill(a, x)
            type(t), intent(inout) :: a(n)
            type(t), intent(in) :: x
            type(t) :: tmp(n)
            tmp = x
            a = tmp
        end subroutine
        subroutine outer(a, x)
            type(t), intent(inout) :: a(n)
            type(t), intent(in) :: x
            call fill(a, x)
        end subroutine
    end template
end module

program template_host_var_06
    use template_host_var_06_m, only: tmpl
    implicit none
    integer :: a(3)
    instantiate tmpl {integer}, only: iouter => outer
    call iouter(a, 4)
    if (any(a /= 4)) error stop 1
    print *, "ok"
end program
