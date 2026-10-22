! Time skewing of a nest of DO loops whose outer loop runs no iteration,
! and whose inner loop takes a bound from a component of an unallocated
! variable: the host must not evaluate the inner bound.
program gpu_metal_555
    implicit none
    integer, parameter :: n = 5000
    real :: u(n), d(n)
    type t
        real, allocatable :: b(:)
    end type
    type(t), allocatable :: x
    integer :: k, it, s, nt
    do k = 1, n
        u(k) = sin(0.01 * k)
    end do
    d = 0
    nt = 0
    do it = 1, nt
        do s = 1, size(x%b)
            do concurrent (k = 2:n - 1)
                d(k) = 0.25 * (u(k - 1) + 2.0 * u(k) + u(k + 1))
            end do
            do concurrent (k = 2:n - 1)
                u(k) = d(k)
            end do
        end do
    end do
    print *, "ok"
end program
