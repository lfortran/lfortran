module template_nonconst_bound_02_mod
    implicit none
    integer :: m = 3
    template tm {n}
        deferred integer, parameter :: n
        type :: w
            ! `m` is not a named constant, so `max(n*m, 5)` is not constant
            integer :: b(max(n*m, 5))
        end type
    end template
end module

program template_nonconst_bound_02
    use template_nonconst_bound_02_mod
    implicit none
end program
