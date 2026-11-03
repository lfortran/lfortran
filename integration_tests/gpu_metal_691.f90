module gpu_metal_691_m
    implicit none
contains
    pure real function centred(a, i, m) result(r)
        real, intent(in) :: a(:)
        integer(8), intent(in) :: i, m
        if (i == 1) then
            r = a(2) - a(1)
        else if (i == m) then
            r = a(m) - a(m - 1)
        else
            r = 0.5 * (a(i + 1) - a(i - 1))
        end if
    end function

    pure real function centred4(a, i, m) result(r)
        real, intent(in) :: a(:)
        integer, intent(in) :: i, m
        if (i == 1) then
            r = a(2) - a(1)
        else if (i == m) then
            r = a(m) - a(m - 1)
        else
            r = 0.5 * (a(i + 1) - a(i - 1))
        end if
    end function

    pure real function item(a, i) result(r)
        real, intent(in) :: a(:)
        integer, intent(in) :: i
        r = a(i)
    end function

    ! Loop fusion inlines `item` of the intermediate array `f` into the
    ! update loop and replaces the read of `f` by the producer's value at
    ! that subscript: an integer(8) subscript for the integer(4) index.
    subroutine fused(q, f, n, steps)
        real, allocatable, intent(inout) :: q(:), f(:)
        integer, intent(in) :: n, steps
        integer :: i, step
        do step = 1, steps
            do concurrent (i = 1:n)
                f(i) = 2 * item(q, i)
            end do
            do concurrent (i = 2:n)
                q(i) = q(i) + item(f, i) - item(f, i - 1)
            end do
        end do
    end subroutine
end module

program gpu_metal_691
    ! Index-set splitting inlines a pure function of an assumed-shape
    ! array into the offloaded loop, with the subscripts of the dummy
    ! shifted by the lower bound of the actual argument. Compiled with
    ! --descriptor-index-64 the kernels compute element offsets in 64
    ! bits, so that shift has to be computed in integer(8) too (#16571):
    ! the lower bounds 2147483645, 3000000000 and 6442450941 do not fit
    ! integer(4). The arrays are small, so the test runs on the device
    ! (cuda_cpu in CI) and on the host.
    use gpu_metal_691_m
    implicit none
    real, allocatable :: x(:), y(:), w(:), q(:), f(:)
    integer(8) :: lo8, i, m, los(4)
    integer :: j, k, m4
    los = [1_8, 2147483645_8, 3000000000_8, 6442450941_8]
    m = 8
    m4 = 8
    do k = 1, 4
        lo8 = los(k)
        allocate(x(lo8:lo8 + m - 1), y(lo8:lo8 + m - 1), w(lo8:lo8 + m - 1))
        do i = lo8, lo8 + m - 1
            x(i) = real((i - lo8 + 1)**2)
        end do
        y = 0
        w = 0
        do concurrent (i = lo8:lo8 + m - 1)
            y(i) = centred(x, i - lo8 + 1, m)
        end do
        do concurrent (j = 1:m4)
            w(lo8 + j - 1) = centred4(x, j, m4)
        end do
        print *, lo8, y
        if (abs(y(lo8) - 3) > 1e-4) error stop 1
        if (abs(y(lo8 + m - 1) - 15) > 1e-4) error stop 2
        if (abs(w(lo8) - 3) > 1e-4) error stop 3
        if (abs(w(lo8 + m - 1) - 15) > 1e-4) error stop 4
        do i = lo8 + 1, lo8 + m - 2
            if (abs(y(i) - 2*(i - lo8 + 1)) > 1e-3) error stop 5
            if (abs(w(i) - 2*(i - lo8 + 1)) > 1e-3) error stop 6
        end do
        deallocate(x, y, w)
    end do
    allocate(q(8), f(8))
    q = [(real(j*j), j = 1, 8)]
    f = 0
    call fused(q, f, 8, 1)
    print *, q
    if (abs(q(1) - 1) > 1e-4) error stop 7
    do j = 2, 8
        if (abs(q(j) - (j*j + 2*(2*j - 1))) > 1e-3) error stop 8
    end do
    print *, "ok"
end program
