module template_nonconst_bound_01_mod
    implicit none
    integer :: m = 3
    template tm {n}
        deferred integer, parameter :: n
        type :: w
            ! `m` is not a named constant, so `n*m` is not constant
            integer :: b(n*m)
        end type
    end template
end module

program template_nonconst_bound_01
    use template_nonconst_bound_01_mod
    implicit none
end program
