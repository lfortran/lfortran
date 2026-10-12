module template_nested_const_m
    implicit none
    template t2 {p}
        deferred integer, parameter :: p
        integer, parameter :: c = p*2
    end template
    template tm {n}
        deferred integer, parameter :: n
    contains
        integer function g()
            instantiate t2 {n}, only: c
            g = c
        end function
    end template
end module

program template_deferred_const_nested_01
    use template_nested_const_m
    implicit none
    instantiate tm {3}, only: g3 => g
    if (g3() /= 6) error stop
end program
