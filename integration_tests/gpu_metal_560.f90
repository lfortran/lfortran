module gpu_metal_560_mod
    implicit none
    type :: inner
        real, allocatable :: q(:)
    end type
    type :: outer
        type(inner) :: in
    end type
contains
    ! A pure subroutine whose INTENT(OUT) dummy has an allocatable
    ! component, which it allocates.
    pure subroutine reset(x, n)
        type(inner), intent(out) :: x
        integer, intent(in) :: n
        integer :: i
        allocate(x%q(n))
        do i = 1, n
            x%q(i) = real(i*i)
        end do
    end subroutine

    ! An impure, self-contained function without input/output that
    ! allocates its INTENT(INOUT) argument the first time.
    integer function init(a, n) result(r)
        real, allocatable, intent(inout) :: a(:)
        integer, intent(in) :: n
        integer :: i
        if (.not. allocated(a)) then
            allocate(a(n))
            do i = 1, n
                a(i) = real(i*i)
            end do
        end if
        r = 0
    end function
end module

program gpu_metal_560
    ! The consumer updates q (and s%q), which the producer reads, so the
    ! fused nest would write a copy of it allocated before the time loop.
    ! But q is allocated only inside the time loop: by a function call;
    ! s%q by an intrinsic assignment of a derived-type object with an
    ! allocatable component; o%in%q by an intrinsic assignment of the
    ! derived-type component o%in, and by a pure subroutine.
    use gpu_metal_560_mod
    implicit none
    type :: t
        real, allocatable :: q(:)
    end type
    type(t) :: s, s0
    type(outer) :: o
    type(inner) :: in0
    real, allocatable :: q(:), f(:)
    real :: expected_q(8), expected_f(8)
    integer :: n, k, i, d
    n = 8
    allocate(f(n), s0%q(n), in0%q(n))
    do i = 1, n
        s0%q(i) = real(i*i)
        in0%q(i) = real(i*i)
    end do
    expected_q = [1.0, 5.1875, 10.46875, 17.5, 26.5, 37.46875, 50.1875, 64.0]
    expected_f = [3.875, 5.125, 7.0, 9.0, 11.0, 12.875, 14.125, -1.0]

    f = -1.0
    do k = 1, 3
        d = init(q, n)
        do concurrent (i = 1:n - 1)
            f(i) = q(i + 1) - q(i)
        end do
        do concurrent (i = 2:n - 1)
            q(i) = q(i) + 0.25 * (f(i) - f(i - 1))
        end do
    end do
    print *, q
    print *, f
    if (any(abs(q - expected_q) > 1e-4)) error stop
    if (any(abs(f - expected_f) > 1e-4)) error stop

    f = -1.0
    do k = 1, 3
        if (k == 1) s = s0
        do concurrent (i = 1:n - 1)
            f(i) = s%q(i + 1) - s%q(i)
        end do
        do concurrent (i = 2:n - 1)
            s%q(i) = s%q(i) + 0.25 * (f(i) - f(i - 1))
        end do
    end do
    print *, s%q
    print *, f
    if (any(abs(s%q - expected_q) > 1e-4)) error stop
    if (any(abs(f - expected_f) > 1e-4)) error stop

    f = -1.0
    do k = 1, 3
        if (k == 1) o%in = in0
        do concurrent (i = 1:n - 1)
            f(i) = o%in%q(i + 1) - o%in%q(i)
        end do
        do concurrent (i = 2:n - 1)
            o%in%q(i) = o%in%q(i) + 0.25 * (f(i) - f(i - 1))
        end do
    end do
    print *, o%in%q
    print *, f
    if (any(abs(o%in%q - expected_q) > 1e-4)) error stop
    if (any(abs(f - expected_f) > 1e-4)) error stop

    deallocate(o%in%q)
    f = -1.0
    do k = 1, 3
        if (k == 1) call reset(o%in, n)
        do concurrent (i = 1:n - 1)
            f(i) = o%in%q(i + 1) - o%in%q(i)
        end do
        do concurrent (i = 2:n - 1)
            o%in%q(i) = o%in%q(i) + 0.25 * (f(i) - f(i - 1))
        end do
    end do
    print *, o%in%q
    print *, f
    if (any(abs(o%in%q - expected_q) > 1e-4)) error stop
    if (any(abs(f - expected_f) > 1e-4)) error stop
end program
