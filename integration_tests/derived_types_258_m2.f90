module derived_types_258_m2
    use derived_types_258_m1
    implicit none
contains
    pure function codes() result(c)
        ! A parameter array of a use-associated derived type. Its elements
        ! are constants of that type, and the module file must name the type
        ! with a symbol this module owns.
        integer :: c(2)
        type(flag), parameter :: out_of_range(2) = [overflow, underflow]
        c(1) = out_of_range(1)%value
        c(2) = out_of_range(2)%value
    end function
end module
