module gpu_metal_512_m
    ! Producer and consumer loops that the fusion of device loops must
    ! leave apart, or fuse and still give the results of the two loops.
    implicit none
contains
    ! The consumer writes the field only where it is positive: the copy of
    ! the field it would write into does not hold every element.
    subroutine update_where_positive(q, f, n, steps)
        real, allocatable, intent(inout) :: q(:), f(:)
        integer, intent(in) :: n, steps
        integer :: step, i
        do step = 1, steps
            do concurrent (i = 1:n - 1)
                f(i) = q(i + 1) - q(i)
            end do
            do concurrent (i = 2:n - 1)
                if (q(i) > 0) q(i) = q(i) + 0.25 * (f(i) - f(i - 1))
            end do
        end do
    end subroutine

    ! The producer reads the array it writes.
    subroutine producer_reads_itself(q, f, n, steps)
        real, allocatable, intent(inout) :: q(:), f(:)
        integer, intent(in) :: n, steps
        integer :: step, i
        do step = 1, steps
            do concurrent (i = 1:n - 1)
                f(i) = q(i + 1) - q(i) + 0.0 * f(n)
            end do
            do concurrent (i = 2:n - 1)
                q(i) = q(i) + 0.25 * (f(i) - f(i - 1))
            end do
        end do
    end subroutine

    ! Stages of a time step that differ by a SELECT CASE, an odd number of
    ! steps, so that the last fused execution leaves its values in the
    ! copy of the field.
    subroutine staged(q, f, n, steps)
        real, allocatable, intent(inout) :: q(:), f(:)
        integer, intent(in) :: n, steps
        integer :: step, stage, i
        do step = 1, steps
            do stage = 1, 3
                do concurrent (i = 1:n - 1)
                    f(i) = q(i + 1) - q(i)
                end do
                do concurrent (i = 2:n - 1)
                    select case (stage)
                    case (1)
                        q(i) = q(i) + 0.1 * (f(i) - f(i - 1))
                    case (2)
                        q(i) = q(i) + 0.2 * (f(i) - f(i - 1))
                    case default
                        q(i) = 0.5 * q(i) + 0.3 * (f(i) - f(i - 1))
                    end select
                end do
            end do
        end do
    end subroutine

    ! Not in a loop: the flux array keeps the values of the one execution.
    subroutine once(q, f, n)
        real, allocatable, intent(inout) :: q(:), f(:)
        integer, intent(in) :: n
        integer :: i
        do concurrent (i = 1:n - 1)
            f(i) = q(i + 1) - q(i)
        end do
        do concurrent (i = 2:n - 1)
            q(i) = q(i) + 0.25 * (f(i) - f(i - 1))
        end do
    end subroutine

    subroutine reference(q, f, n, steps, kind)
        real, intent(inout) :: q(:), f(:)
        integer, intent(in) :: n, steps, kind
        real :: qn(n)
        real :: w(3)
        integer :: step, stage, i, nstages
        w = [0.1, 0.2, 0.3]
        nstages = merge(3, 1, kind == 3)
        do step = 1, steps
            do stage = 1, nstages
                do i = 1, n - 1
                    f(i) = q(i + 1) - q(i)
                end do
                qn = q
                do i = 2, n - 1
                    select case (kind)
                    case (1)
                        if (q(i) > 0) qn(i) = q(i) + 0.25 * (f(i) - f(i - 1))
                    case (2)
                        qn(i) = q(i) + 0.25 * (f(i) - f(i - 1))
                    case (3)
                        if (stage < 3) then
                            qn(i) = q(i) + w(stage) * (f(i) - f(i - 1))
                        else
                            qn(i) = 0.5 * q(i) + w(stage) * (f(i) - f(i - 1))
                        end if
                    end select
                end do
                q = qn
            end do
        end do
    end subroutine
end module

program gpu_metal_512
    use gpu_metal_512_m
    implicit none
    integer, parameter :: n = 50
    real, allocatable :: q(:), f(:), q0(:)
    real :: qh(n), fh(n)
    integer :: i

    allocate(q(n), f(n), q0(n))
    do i = 1, n
        q0(i) = sin(0.4 * i) + 0.2
    end do

    q = q0; f = -3; qh = q0; fh = -3
    call update_where_positive(q, f, n, 7)
    call reference(qh, fh, n, 7, 1)
    call check(q, qh, 1)
    call check(f, fh, 2)

    q = q0; f = -3; qh = q0; fh = -3
    call producer_reads_itself(q, f, n, 7)
    call reference(qh, fh, n, 7, 2)
    call check(q, qh, 3)
    call check(f, fh, 4)

    q = q0; f = -3; qh = q0; fh = -3
    call staged(q, f, n, 3)
    call reference(qh, fh, n, 3, 3)
    call check(q, qh, 5)
    call check(f, fh, 6)

    q = q0; f = -3; qh = q0; fh = -3
    call once(q, f, n)
    call reference(qh, fh, n, 1, 2)
    call check(q, qh, 7)
    call check(f, fh, 8)

    ! A flux array whose lower bound is not the first index the flux loop
    ! writes.
    deallocate(f)
    allocate(f(0:n))
    q = q0; f = -3; qh = q0; fh = -3
    call staged(q, f, n, 2)
    call reference(qh, fh, n, 2, 3)
    call check(q, qh, 9)
    print *, "ok"
contains
    subroutine check(a, b, code)
        real, intent(in) :: a(:), b(:)
        integer, intent(in) :: code
        if (maxval(abs(a - b)) > 1.0e-5 * max(1.0, maxval(abs(b)))) then
            print *, code, maxval(abs(a - b))
            error stop code
        end if
    end subroutine
end program
