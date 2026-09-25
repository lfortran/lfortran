module template_broadcast_01_m
    implicit none
    template tm {t}
        deferred type :: t
    contains
        function twice(x) result(y)
            type(t), intent(in) :: x
            type(t) :: y(2)
            y = x
        end function
    end template
end module

program template_broadcast_01
    use template_broadcast_01_m
    implicit none
    instantiate tm {integer}, only: twice_i => twice
    instantiate tm {real}, only: twice_r => twice
    integer :: a(2)
    real :: b(2)
    a = twice_i(7)
    if (any(a /= 7)) error stop
    b = twice_r(2.5)
    if (any(abs(b - 2.5) > 1e-6)) error stop
    print *, a
    print *, b
end program
