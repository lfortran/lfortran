module gpu_metal_484_m
    implicit none
contains
    pure function smooth(v) result(r)
        real, intent(in) :: v(:)
        real, allocatable :: r(:)
        integer :: i
        allocate(r(size(v)))
        do i = 1, size(v)
            r(i) = 0.5*v(i)
            if (i > 1) r(i) = r(i) + 0.25*v(i-1)
            if (i < size(v)) r(i) = r(i) + 0.25*v(i+1)
        end do
    end function

    pure function scaled(v, c) result(r)
        real, intent(in) :: v(:), c
        real, allocatable :: r(:)
        allocate(r(size(v)))
        r = c*v + 1.0
    end function

    ! Launches the same kernels, with per-thread workspaces sized by `m`,
    ! `steps` times each, with no host access between the launches.
    subroutine run(m, ncol, steps, a, b)
        integer, intent(in) :: m, ncol, steps
        real, intent(inout) :: a(m, ncol), b(m, ncol)
        integer :: s, j
        do s = 1, steps
            do concurrent (j = 1:ncol)
                b(:, j) = smooth(a(:, j))
            end do
            do concurrent (j = 1:ncol)
                a(:, j) = scaled(b(:, j), 0.5)
            end do
        end do
    end subroutine

    subroutine run_ref(m, ncol, steps, a, b)
        integer, intent(in) :: m, ncol, steps
        real, intent(inout) :: a(m, ncol), b(m, ncol)
        integer :: s, j
        do s = 1, steps
            do j = 1, ncol
                b(:, j) = smooth(a(:, j))
            end do
            do j = 1, ncol
                a(:, j) = scaled(b(:, j), 0.5)
            end do
        end do
    end subroutine
end module

program gpu_metal_484
    use gpu_metal_484_m
    implicit none
    integer, parameter :: nsizes = 7
    integer, parameter :: sizes(nsizes) = [8, 40, 3, 72, 0, 16, 72]
    integer, parameter :: ncols(nsizes) = [5, 33, 64, 9, 4, 1, 17]
    real, allocatable :: a(:, :), b(:, :), ra(:, :), rb(:, :)
    integer :: k, i, j, m, ncol

    do k = 1, nsizes
        m = sizes(k)
        ncol = ncols(k)
        allocate(a(m, ncol), b(m, ncol), ra(m, ncol), rb(m, ncol))
        do j = 1, ncol
            do i = 1, m
                a(i, j) = real(mod(i + 3*j + k, 11))
            end do
        end do
        b = 0
        ra = a
        rb = b
        call run(m, ncol, 25, a, b)
        call run_ref(m, ncol, 25, ra, rb)
        if (any(abs(a - ra) > 1e-5)) error stop
        if (any(abs(b - rb) > 1e-5)) error stop
        deallocate(a, b, ra, rb)
    end do
    print *, "ok"
end program
