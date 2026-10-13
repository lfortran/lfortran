! A deferred constant declared after a nested template in the same template
! is still a deferred argument of the enclosing template.
module template_nested_deferred_02_m
    implicit none
    template outer {t, n}
        template inner {u}
            deferred type :: u
        contains
            function id(x) result(r)
                type(u), intent(in) :: x
                type(u) :: r
                r = x
            end function
        end template
        deferred type :: t
        deferred integer, parameter :: n
    contains
        function last(x) result(r)
            type(t), intent(in) :: x(n)
            type(t) :: r
            r = x(n)
        end function
    end template
end module template_nested_deferred_02_m

program template_nested_deferred_02
    use template_nested_deferred_02_m
    implicit none
    instantiate outer {integer, 3}, only: ilast => last, iinner => inner
    instantiate iinner {real}, only: rid => id
    if (ilast([4, 5, 6]) /= 6) error stop 1
    if (abs(rid(2.5) - 2.5) > 1e-6) error stop 2
    print *, "ok"
end program template_nested_deferred_02
