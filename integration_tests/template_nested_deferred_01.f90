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
            function g2(x, y) result(r)
                type(u), intent(in) :: x(:)
                type(t), intent(in) :: y(:)
                type(u) :: r
                type(t) :: s
                s = y(2)
                r = idu(x(2))
            end function
            function idu(a) result(b)
                type(u), intent(in) :: a
                type(u) :: b
                b = a
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
            function h2(x) result(r)
                type(t), intent(in) :: x(:)
                type(t) :: r
                r = idt(x(2))
            end function
            function idt(a) result(b)
                type(t), intent(in) :: a
                type(t) :: b
                b = a
            end function
        end template
    end template
end module template_nested_deferred_01_m
program template_nested_deferred_01
    use template_nested_deferred_01_m
    implicit none
    instantiate outer {real}, only: iinner => inner, ishadow => shadow
    instantiate iinner {integer}, only: g => g, g2 => g2
    instantiate ishadow {integer}, only: h => h, h2 => h2
    if (g(3, 2.5) /= 3) error stop
    if (g2([3, 4], [1.5, 2.5]) /= 4) error stop
    if (h(7) /= 7) error stop
    if (h2([7, 8]) /= 8) error stop
    print *, "ok"
end program template_nested_deferred_01
