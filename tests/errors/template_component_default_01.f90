! A component default initializer in a template may use the template's
! deferred constants, but not a variable.
module template_component_default_01_m
    implicit none
    integer :: v = 10
    template tm {n}
        deferred integer, parameter :: n
        type :: w
            integer :: k = n + v
        end type
    end template
end module

program template_component_default_01
    use template_component_default_01_m
    implicit none
    instantiate tm {3}, only: w3 => w
    type(w3) :: s
    print *, s%k
end program
