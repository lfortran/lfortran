module template_reshape_01_m
    implicit none
    template tm {n}
        deferred integer, parameter :: n
    contains
        subroutine f()
            integer :: x(n, n)
            real :: y(n, 1, n)
            x = reshape([1, 2, 3, 4], [2, 2])
            if (x(2, 1) /= 2 .or. x(1, 2) /= 3) error stop
            if (any(x /= reshape([1, 2, 3, 4], [n, n]))) error stop
            y = reshape([1.0, 2.0, 3.0, 4.0], [2, 1, 2])
            if (abs(y(2, 1, 2) - 4.0) > 1e-6) error stop
            print *, x, y
        end subroutine
    end template
end module

program template_reshape_01
    use template_reshape_01_m
    implicit none
    instantiate tm {2}, only: f
    call f()
end program
