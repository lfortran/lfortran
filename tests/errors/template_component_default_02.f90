! A component default initializer in a template that uses a deferred
! constant must reduce to a compile time constant in each instantiation.
module template_component_default_02_g
    implicit none
contains
    integer function g()
        g = 1
    end function
end module

module template_component_default_02_m
    use template_component_default_02_g
    implicit none
    template tm {n}
        deferred integer, parameter :: n
        type :: w
            integer :: k = n + 1
            integer :: j = n + g()
        end type
    end template
end module

program template_component_default_02
    use template_component_default_02_m
    implicit none
    instantiate tm {3}, only: w3 => w
    type(w3) :: s
    print *, s%k, s%j
end program
