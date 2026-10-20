module gpu_metal_510_m
    ! Face fluxes of a scalar with one-sided rows at the boundaries, and
    ! their divergence, as pure functions of the whole arrays.
    implicit none
contains
    pure real function face_flux(q, n, i, j) result(f)
        real, intent(in) :: q(:,:)
        integer, intent(in) :: n, i, j
        if (i == 1) then
            f = q(2, j) - q(1, j)
        else if (i < n) then
            f = 0.75 * (q(i + 1, j) - q(i, j)) + 0.125 * (q(i, j) - q(i - 1, j))
        else
            f = q(n, j) - q(n - 1, j)
        end if
    end function

    pure real function divergence(flux, n, i, j) result(d)
        real, intent(in) :: flux(:,:)
        integer, intent(in) :: n, i, j
        if (i == 1) then
            d = flux(2, j) - flux(1, j)
        else
            d = flux(i, j) - flux(i - 1, j)
        end if
    end function

    ! The time loop, with a flux loop and an update loop per step: the
    ! update writes the field the fluxes read.
    subroutine advance(q, flux, n, m, steps, dt)
        real, allocatable, intent(inout) :: q(:,:)
        real, allocatable, intent(inout) :: flux(:,:)
        integer, intent(in) :: n, m, steps
        real, intent(in) :: dt
        integer :: step, i, j
        do step = 1, steps
            do concurrent (j = 1:m, i = 1:n)
                flux(i, j) = 0.5 * face_flux(q, n, i, j)
            end do
            do concurrent (j = 1:m, i = 2:n - 1)
                q(i, j) = q(i, j) - dt * divergence(flux, n, i, j)
            end do
        end do
    end subroutine

    subroutine advance_host(q, flux, n, m, steps, dt)
        real, intent(inout) :: q(:,:), flux(:,:)
        integer, intent(in) :: n, m, steps
        real, intent(in) :: dt
        real :: qn(n, m)
        integer :: step, i, j
        do step = 1, steps
            do j = 1, m
                do i = 1, n
                    flux(i, j) = 0.5 * face_flux(q, n, i, j)
                end do
            end do
            qn = q
            do j = 1, m
                do i = 2, n - 1
                    qn(i, j) = q(i, j) - dt * divergence(flux, n, i, j)
                end do
            end do
            q = qn
        end do
    end subroutine

    ! The update reads one element (flux(1, j), through the boundary
    ! row of `divergence`) that the flux loop does not write: the loops
    ! must not be fused, and that element keeps its value.
    subroutine advance_partial(q, flux, n, m, steps, dt)
        real, allocatable, intent(inout) :: q(:,:)
        real, allocatable, intent(inout) :: flux(:,:)
        integer, intent(in) :: n, m, steps
        real, intent(in) :: dt
        integer :: step, i, j
        do step = 1, steps
            do concurrent (j = 1:m, i = 2:n)
                flux(i, j) = 0.5 * face_flux(q, n, i, j)
            end do
            do concurrent (j = 1:m, i = 1:n - 1)
                q(i, j) = q(i, j) - dt * divergence(flux, n, i, j)
            end do
        end do
    end subroutine

    subroutine advance_partial_host(q, flux, n, m, steps, dt)
        real, intent(inout) :: q(:,:), flux(:,:)
        integer, intent(in) :: n, m, steps
        real, intent(in) :: dt
        real :: qn(n, m)
        integer :: step, i, j
        do step = 1, steps
            do j = 1, m
                do i = 2, n
                    flux(i, j) = 0.5 * face_flux(q, n, i, j)
                end do
            end do
            qn = q
            do j = 1, m
                do i = 1, n - 1
                    qn(i, j) = q(i, j) - dt * divergence(flux, n, i, j)
                end do
            end do
            q = qn
        end do
    end subroutine

    ! The time loop also sets a boundary value of the field: the update's
    ! copy of the field cannot take turns with it, and is copied back.
    subroutine advance_boundary(q, flux, n, m, steps, dt)
        real, allocatable, intent(inout) :: q(:,:)
        real, allocatable, intent(inout) :: flux(:,:)
        integer, intent(in) :: n, m, steps
        real, intent(in) :: dt
        integer :: step, i, j
        do step = 1, steps
            do concurrent (j = 1:m, i = 1:n)
                flux(i, j) = 0.5 * face_flux(q, n, i, j)
            end do
            do concurrent (j = 1:m, i = 2:n - 1)
                q(i, j) = q(i, j) - dt * divergence(flux, n, i, j)
            end do
            do concurrent (j = 1:m)
                q(1, j) = q(2, j)
            end do
        end do
    end subroutine

    ! A loop that can end early: the last step that runs is not known
    ! before it runs.
    subroutine advance_until(q, flux, n, m, steps, dt, limit)
        real, allocatable, intent(inout) :: q(:,:)
        real, allocatable, intent(inout) :: flux(:,:)
        integer, intent(in) :: n, m, steps
        real, intent(in) :: dt, limit
        integer :: step, i, j
        do step = 1, steps
            do concurrent (j = 1:m, i = 1:n)
                flux(i, j) = 0.5 * face_flux(q, n, i, j)
            end do
            do concurrent (j = 1:m, i = 2:n - 1)
                q(i, j) = q(i, j) - dt * divergence(flux, n, i, j)
            end do
            if (q(2, 1) < limit) exit
        end do
    end subroutine
end module

program gpu_metal_510
    ! Fusing a loop that computes an array into the loop after it that
    ! reads it gives the results of the two loops, including the values
    ! the array is left with, whether the loops are fused or not.
    use gpu_metal_510_m
    implicit none
    integer, parameter :: n = 45, m = 7, steps = 6
    real, allocatable :: q(:,:), flux(:,:), q0(:,:)
    real :: qh(n, m), fh(n, m)
    integer :: i, j

    allocate(q(n, m), flux(n, m), q0(n, m))
    do j = 1, m
        do i = 1, n
            q0(i, j) = sin(0.3 * i) + 0.1 * j + 0.01 * mod(i * j, 7)
        end do
    end do

    q = q0
    flux = -7
    qh = q0
    fh = -7
    call advance(q, flux, n, m, steps, 0.1)
    call advance_host(qh, fh, n, m, steps, 0.1)
    call check(q, qh, 1)
    call check(flux, fh, 2)

    q = q0
    flux = -7
    qh = q0
    fh = -7
    call advance_partial(q, flux, n, m, steps, 0.1)
    call advance_partial_host(qh, fh, n, m, steps, 0.1)
    call check(q, qh, 3)
    call check(flux, fh, 4)
    if (any(flux(1, :) /= -7)) error stop 5

    q = q0
    flux = -7
    call advance_until(q, flux, n, m, 50, 0.1, q0(2, 1) - 0.05)
    qh = q0
    fh = -7
    do i = 1, 50
        call advance_host(qh, fh, n, m, 1, 0.1)
        if (qh(2, 1) < q0(2, 1) - 0.05) exit
    end do
    call check(q, qh, 6)
    call check(flux, fh, 7)

    q = q0
    flux = -7
    call advance_boundary(q, flux, n, m, steps, 0.1)
    qh = q0
    fh = -7
    do i = 1, steps
        call advance_host(qh, fh, n, m, 1, 0.1)
        qh(1, :) = qh(2, :)
    end do
    call check(q, qh, 11)
    call check(flux, fh, 12)

    ! A flux array with an element the flux loop does not write: the run
    ! time check of its bounds keeps the loops apart, and that element
    ! keeps its value.
    deallocate(flux)
    allocate(flux(n + 1, m))
    q = q0
    flux = -7
    call advance(q, flux, n, m, steps, 0.1)
    qh = q0
    fh = -7
    call advance_host(qh, fh, n, m, steps, 0.1)
    call check(q, qh, 8)
    call check(flux(1:n, :), fh, 9)
    if (any(flux(n + 1, :) /= -7)) error stop 10
    print *, "ok"
contains
    subroutine check(a, b, code)
        real, intent(in) :: a(:,:), b(:,:)
        integer, intent(in) :: code
        if (maxval(abs(a - b)) > 1.0e-5 * max(1.0, maxval(abs(b)))) then
            print *, code, maxval(abs(a - b))
            error stop code
        end if
    end subroutine
end program
