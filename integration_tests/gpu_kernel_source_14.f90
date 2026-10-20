program gpu_kernel_source_14
    ! Every iteration defines `t` whole before it references it, so `t`
    ! is private to the iteration, and so is the temporary an array
    ! constructor is lowered to.  Each kernel must declare them as
    ! locals of the thread, not take them as buffers every thread shares:
    ! CI has no GPU that would expose the race, so the reference kernel
    ! source is what checks this.  The second loop names `u` in a LOCAL
    ! locality specifier and defines it element by element.
    implicit none
    integer, parameter :: n = 8
    real :: x(n), g(4, n), t(4), u(4)
    integer :: j

    do j = 1, n
        x(j) = real(j)
    end do
    do concurrent (j = 1:n)
        associate (e => 10.0 * x(j))
            t = [x(j), e, e + 1.0, e + 2.0]
            g(:, j) = t
        end associate
    end do
    do j = 1, n
        if (abs(g(1, j) - x(j)) > 1.0e-5) error stop 1
        if (abs(g(2, j) - 10.0 * x(j)) > 1.0e-5) error stop 2
        if (abs(g(3, j) - (10.0 * x(j) + 1.0)) > 1.0e-5) error stop 3
        if (abs(g(4, j) - (10.0 * x(j) + 2.0)) > 1.0e-5) error stop 4
    end do
    do concurrent (j = 1:n) local(u)
        t = [x(j), 2.0 * x(j), 3.0 * x(j), 4.0 * x(j)]
        u(1) = t(4)
        u(2) = t(3)
        u(3) = t(2)
        u(4) = t(1)
        g(:, j) = u
    end do
    do j = 1, n
        if (abs(g(1, j) - 4.0 * x(j)) > 1.0e-5) error stop 5
        if (abs(g(2, j) - 3.0 * x(j)) > 1.0e-5) error stop 6
        if (abs(g(3, j) - 2.0 * x(j)) > 1.0e-5) error stop 7
        if (abs(g(4, j) - x(j)) > 1.0e-5) error stop 8
    end do
    print *, "ok"
end program
