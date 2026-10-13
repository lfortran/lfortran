module template_deferred_const_21_m
    implicit none
    template t2 {p}
        deferred integer, parameter :: p
        integer, parameter :: c = p*2
        integer, parameter :: d = c + 1
    contains
        integer function h()
            h = c*10
        end function
    end template
    ! The deferred constant `n` of `tm` is an instantiation argument, so the
    ! named constants of `t2` get their values when `tm` is instantiated.
    template tm {n}
        deferred integer, parameter :: n
        instantiate t2 {n}, only: cs => c
    contains
        integer function g()
            instantiate t2 {n}, only: c, d, h
            integer :: x(d)
            g = c*100 + size(x)*10 + cs + h()
        end function
    end template
end module

program template_deferred_const_21
    use template_deferred_const_21_m
    implicit none
    instantiate tm {3}, only: g3 => g
    instantiate tm {4}, only: g4 => g
    print *, g3(), g4()
    if (g3() /= 6*100 + 7*10 + 6 + 60) error stop
    if (g4() /= 8*100 + 9*10 + 8 + 80) error stop
end program
