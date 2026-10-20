module gpu_metal_535_mod
    implicit none
    real :: c
    integer :: steps, mt
end module

! Time skewing of a loop whose host statements assign variables of a
! module, and whose DO variable is a variable of a module: the skewed
! loop saves and restores them for every band.
program gpu_metal_535
    use gpu_metal_535_mod
    implicit none
    integer, parameter :: n = 5003
    real :: u(n), d(n), r(n), e(n), cr
    integer :: it, k, sr
    do k = 1, n
        u(k) = sin(0.01 * k) + 0.1 * mod(k, 5)
    end do
    r = u
    c = 0.5
    steps = 0
    do mt = 1, 6
        c = 0.9 * c
        steps = steps + 1
        do concurrent (k = 2:n)
            d(k) = u(k) - c * (u(k) - u(k - 1))
        end do
        do concurrent (k = 2:n)
            u(k) = d(k)
        end do
    end do
    cr = 0.5
    sr = 0
    do it = 1, 6
        cr = 0.9 * cr
        sr = sr + 1
        do k = 2, n
            e(k) = r(k) - cr * (r(k) - r(k - 1))
        end do
        do k = 2, n
            r(k) = e(k)
        end do
    end do
    print *, maxval(abs(u - r)), c, steps
    if (maxval(abs(u - r)) > 1e-5) error stop 1
    if (abs(c - cr) > 1e-6) error stop 2
    if (steps /= 6 .or. sr /= 6) error stop 3
    print *, "ok"
end program
