module gpu_kernel_source_16_m
implicit none
contains
    integer function bump(m)
        integer, intent(inout) :: m
        m = 2
        bump = 0
    end function
end module

program gpu_kernel_source_16
    ! Arrays that must stay buffers every thread shares, which the
    ! reference kernel source checks: making either a local of the thread
    ! would change what the program computes.
    use gpu_kernel_source_16_m
    implicit none
    integer, parameter :: n = 8
    real :: t(2), u(2), x(n), g(n)
    integer :: j, k, y

    do j = 1, n
        x(j) = real(j)
    end do

    ! Each iteration defines `t` whole, but after the loop `t(1)` is read
    ! before it is defined again: the call changes `k`, so `t(k) = 5.0`
    ! defines `t(2)`, not `t(1)`, and `t(1)` keeps the value the loop
    ! left there.
    do concurrent (j = 1:1)
        t = x(j) + 41.0
        g(j) = t(1)
    end do
    k = 1
    y = bump(k)
    t(k) = 5.0
    t(2) = 5.0
    if (y /= 0 .or. k /= 2) error stop 1
    if (abs(t(1) - 42.0) > 1.0e-5) error stop 2

    ! Each iteration defines only `u(1)`, the same element twice, and
    ! then reads `u(2)`, which keeps the value it had before the loop.
    u(2) = 9.0
    do concurrent (j = 1:n)
        k = 1
        u(k) = 1.0
        u(k + 1 - 1) = 1.0
        g(j) = u(1) + u(2) + x(j)
    end do
    do j = 1, n
        if (abs(g(j) - (10.0 + x(j))) > 1.0e-5) error stop 3
    end do
    print *, "ok"
end program
