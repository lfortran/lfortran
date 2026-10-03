! A host-module variable used as an explicit-shape bound of a dummy of a
! template procedure: the instantiation must call an instantiated copy of
! the bound's getter, not the one in the template.
module template_host_var_06_m
    implicit none
    integer :: n = 3
    template tmpl {t}
        deferred type :: t
    contains
        subroutine fill(a, b, x)
            type(t), intent(inout) :: a(n)
            type(t), intent(inout) :: b(n + 1, 2)
            type(t), intent(in) :: x
            a = x
            b = x
        end subroutine
    end template
end module

program template_host_var_06
    use template_host_var_06_m, only: tmpl, n
    implicit none
    integer :: a(3), b(4, 2)
    real :: r(4), s(5, 2)
    instantiate tmpl {integer}, only: ifill => fill
    instantiate tmpl {real}
    call ifill(a, b, 3)
    if (any(a /= 3)) error stop 1
    if (any(b /= 3)) error stop 2
    n = 4
    r = 0
    call fill(r, s, 2.5)
    if (any(r /= 2.5)) error stop 3
    if (any(s /= 2.5)) error stop 4
    print *, "ok"
end program
