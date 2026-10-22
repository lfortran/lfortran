! Time skewing of a time loop whose first parallel loop runs under a host
! test that never holds, with a bound taken from a component of an
! unallocated variable: the host must not evaluate that bound before the
! loop.
program gpu_metal_538
    implicit none
    integer, parameter :: n = 5000
    real :: u(n), d(n), r(n), e(n)
    type t
        real, allocatable :: b(:)
    end type
    type(t), allocatable :: x
    logical :: have
    integer :: k, it
    do k = 1, n
        u(k) = sin(0.01 * k)
    end do
    r = u
    d = 0
    e = 0
    have = .false.
    do it = 1, 6
        if (have) then
            do concurrent (k = 2:size(x%b) - 1)
                d(k) = 0.0
            end do
        end if
        do concurrent (k = 2:n - 1)
            d(k) = 0.25 * (u(k - 1) + 2.0 * u(k) + u(k + 1))
        end do
        do concurrent (k = 2:n - 1)
            u(k) = d(k)
        end do
    end do
    do it = 1, 6
        do k = 2, n - 1
            e(k) = 0.25 * (r(k - 1) + 2.0 * r(k) + r(k + 1))
        end do
        do k = 2, n - 1
            r(k) = e(k)
        end do
    end do
    if (maxval(abs(u - r)) > 1e-5) error stop 1
    print *, "ok"
end program
