module gpu_metal_513_m
    implicit none
contains
    pure real function grad_x(q, nx, i, j) result(g)
        real, intent(in) :: q(:,:)
        integer, intent(in) :: nx, i, j
        if (i == 1) then
            g = -1.5 * q(1, j) + 2.0 * q(2, j) - 0.5 * q(3, j)
        else if (i + 1 < nx) then
            g = (q(i - 1, j) - 27.0 * q(i, j) + 27.0 * q(i + 1, j) &
                - q(i + 2, j)) / 24.0
        else
            g = 0.5 * q(nx - 2, j) - 2.0 * q(nx - 1, j) + 1.5 * q(nx, j)
        end if
    end function grad_x

    pure real function grad_y(q, ny, i, j) result(g)
        real, intent(in) :: q(:,:)
        integer, intent(in) :: ny, i, j
        if (j == 1) then
            g = -1.5 * q(i, 1) + 2.0 * q(i, 2) - 0.5 * q(i, 3)
        else if (j + 1 < ny) then
            g = (q(i, j - 1) - 27.0 * q(i, j) + 27.0 * q(i, j + 1) &
                - q(i, j + 2)) / 24.0
        else
            g = 0.5 * q(i, ny - 2) - 2.0 * q(i, ny - 1) + 1.5 * q(i, ny)
        end if
    end function grad_y

    ! Two-stage time stepping of a diffusion problem: faces, then the
    ! update of the interior (the boundary keeps its values).
    subroutine advance(q, nx, ny, steps, dt)
        real, intent(inout) :: q(:,:)
        integer, intent(in) :: nx, ny, steps
        real, intent(in) :: dt
        real, allocatable :: fx(:,:), fy(:,:), base(:,:), acc(:,:)
        integer :: step, stage, i, j
        real :: r
        allocate(fx(nx - 1, ny), fy(nx, ny - 1), base(nx, ny), acc(nx, ny))
        do concurrent (j = 1:ny, i = 1:nx)
            base(i, j) = q(i, j)
            acc(i, j) = 0
        end do
        do step = 1, steps
            do stage = 1, 2
                do concurrent (j = 1:ny, i = 1:nx)
                    if (i < nx) fx(i, j) = grad_x(q, nx, i, j)
                    if (j < ny) fy(i, j) = grad_y(q, ny, i, j)
                end do
                do concurrent (j = 2:ny - 1, i = 2:nx - 1) local(r)
                    r = (fx(i, j) - fx(i - 1, j)) + (fy(i, j) - fy(i, j - 1))
                    select case (stage)
                    case (1)
                        acc(i, j) = r
                        q(i, j) = base(i, j) + dt * r
                    case default
                        base(i, j) = base(i, j) + 0.5 * dt * (acc(i, j) + r)
                        q(i, j) = base(i, j)
                    end select
                end do
            end do
        end do
    end subroutine advance

    subroutine advance_ref(q, nx, ny, steps, dt)
        real, intent(inout) :: q(:,:)
        integer, intent(in) :: nx, ny, steps
        real, intent(in) :: dt
        real, allocatable :: fx(:,:), fy(:,:), base(:,:), acc(:,:)
        integer :: step, stage, i, j
        real :: r
        allocate(fx(nx - 1, ny), fy(nx, ny - 1), base(nx, ny), acc(nx, ny))
        base = q
        acc = 0
        do step = 1, steps
            do stage = 1, 2
                do j = 1, ny
                    do i = 1, nx
                        if (i < nx) fx(i, j) = grad_x(q, nx, i, j)
                        if (j < ny) fy(i, j) = grad_y(q, ny, i, j)
                    end do
                end do
                do j = 2, ny - 1
                    do i = 2, nx - 1
                        r = (fx(i, j) - fx(i - 1, j)) &
                            + (fy(i, j) - fy(i, j - 1))
                        if (stage == 1) then
                            acc(i, j) = r
                            q(i, j) = base(i, j) + dt * r
                        else
                            base(i, j) = base(i, j) &
                                + 0.5 * dt * (acc(i, j) + r)
                            q(i, j) = base(i, j)
                        end if
                    end do
                end do
            end do
        end do
    end subroutine advance_ref

    ! One dimension, one-sided boundaries, and a host scalar that changes
    ! every iteration.
    subroutine smooth(u, n, iters)
        real, intent(inout) :: u(:)
        integer, intent(in) :: n, iters
        real, allocatable :: d(:)
        real :: alpha
        integer :: it, k
        allocate(d(n))
        alpha = 0.2
        do it = 1, iters
            do concurrent (k = 1:n)
                if (k == 1) then
                    d(k) = u(2) - u(1)
                else if (k == n) then
                    d(k) = u(n - 1) - u(n)
                else
                    d(k) = u(k - 1) - 2.0 * u(k) + u(k + 1)
                end if
            end do
            do concurrent (k = 1:n)
                u(k) = u(k) + alpha * d(k)
            end do
            alpha = alpha * 0.9
        end do
    end subroutine smooth

    subroutine smooth_ref(u, n, iters)
        real, intent(inout) :: u(:)
        integer, intent(in) :: n, iters
        real, allocatable :: d(:)
        real :: alpha
        integer :: it, k
        allocate(d(n))
        alpha = 0.2
        do it = 1, iters
            do k = 1, n
                if (k == 1) then
                    d(k) = u(2) - u(1)
                else if (k == n) then
                    d(k) = u(n - 1) - u(n)
                else
                    d(k) = u(k - 1) - 2.0 * u(k) + u(k + 1)
                end if
            end do
            do k = 1, n
                u(k) = u(k) + alpha * d(k)
            end do
            alpha = alpha * 0.9
        end do
    end subroutine smooth_ref

    ! A read of a row far from the one written: the loops depend on all of
    ! each other's rows and run as written.
    subroutine pull(u, n, iters)
        real, intent(inout) :: u(:)
        integer, intent(in) :: n, iters
        real, allocatable :: d(:)
        integer :: it, k
        allocate(d(n))
        do it = 1, iters
            do concurrent (k = 1:n)
                d(k) = 0.5 * (u(k) + u(n + 1 - k))
            end do
            do concurrent (k = 1:n)
                u(k) = d(k) + 0.25
            end do
        end do
    end subroutine pull

    subroutine pull_ref(u, n, iters)
        real, intent(inout) :: u(:)
        integer, intent(in) :: n, iters
        real, allocatable :: d(:)
        integer :: it, k
        allocate(d(n))
        do it = 1, iters
            do k = 1, n
                d(k) = 0.5 * (u(k) + u(n + 1 - k))
            end do
            do k = 1, n
                u(k) = d(k) + 0.25
            end do
        end do
    end subroutine pull_ref

    subroutine check2(a, b, what)
        real, intent(in) :: a(:,:), b(:,:)
        character(len=*), intent(in) :: what
        call verdict(maxval(abs(a - b)), maxval(abs(b)), what)
    end subroutine check2

    subroutine check1(a, b, what)
        real, intent(in) :: a(:), b(:)
        character(len=*), intent(in) :: what
        call verdict(maxval(abs(a - b)), maxval(abs(b)), what)
    end subroutine check1

    subroutine verdict(err, scale, what)
        real, intent(in) :: err, scale
        character(len=*), intent(in) :: what
        print *, what, err
        if (.not. (err <= 1.0e-5 * max(scale, 1.0))) error stop
    end subroutine verdict
end module gpu_metal_513_m

program gpu_metal_513
    use gpu_metal_513_m
    implicit none
    integer, parameter :: nx = 37, ny = 53, n = 5003
    real, allocatable :: q(:,:), q_ref(:,:), u(:), u_ref(:)
    integer :: i, j, k, steps
    allocate(q(nx, ny), q_ref(nx, ny), u(n), u_ref(n))
    do steps = 1, 9, 4
        do j = 1, ny
            do i = 1, nx
                q(i, j) = sin(0.3 * i) * cos(0.17 * j) + 0.01 * mod(i * j, 7)
            end do
        end do
        q_ref = q
        call advance(q, nx, ny, steps, 0.05)
        call advance_ref(q_ref, nx, ny, steps, 0.05)
        call check2(q, q_ref, "advance")
    end do
    do k = 1, n
        u(k) = sin(0.01 * k) + 0.1 * mod(k, 5)
    end do
    u_ref = u
    call smooth(u, n, 11)
    call smooth_ref(u_ref, n, 11)
    call check1(u, u_ref, "smooth")
    call pull(u, n, 5)
    call pull_ref(u_ref, n, 5)
    call check1(u, u_ref, "pull")
    print *, "ok"
end program gpu_metal_513
