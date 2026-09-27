module template_nested_deferred_01_m
    implicit none
    template outer {t}
        deferred type :: t
        template inner {u}
            deferred type :: u
        contains
            function g(x, y) result(r)
                type(u), intent(in) :: x
                type(t), intent(in) :: y
                type(u) :: r
                r = x
            end function
        end template
        template shadow {t}
            deferred type :: t
        contains
            function h(x) result(r)
                type(t), intent(in) :: x
                type(t) :: r
                r = x
            end function
        end template
    end template
end module template_nested_deferred_01_m
program template_nested_deferred_01
    use template_nested_deferred_01_m
    implicit none
    instantiate outer {real}, only: iinner => inner, ishadow => shadow
    instantiate iinner {integer}, only: g => g
    instantiate ishadow {integer}, only: h => h
    if (g(3, 2.5) /= 3) error stop
    if (h(7) /= 7) error stop
    print *, "ok"
end program template_nested_deferred_01
