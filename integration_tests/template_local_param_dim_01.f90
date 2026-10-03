module template_local_param_dim_01_m
    implicit none
    template tm {n}
        deferred integer, parameter :: n
    contains
        subroutine f()
            integer, parameter :: k = n
            integer, parameter :: m = k + 1
            integer :: x(k), y(m)
            x = [1, 2]
            x = cshift(x, 1)
            if (any(x /= [2, 1])) error stop
            y = [1, 2, 3]
            y = eoshift(y, 1)
            if (any(y /= [2, 3, 0])) error stop
            print *, x, y
        end subroutine
    end template
end module

program template_local_param_dim_01
    use template_local_param_dim_01_m
    implicit none
    instantiate tm {2}, only: f
    call f()
end program
