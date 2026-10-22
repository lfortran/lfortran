! Time skewing must see the arrays a pure function that is not inlined
! (it has a local array) reads by use association.
module gpu_metal_531_m
    implicit none
    integer, parameter :: n = 5003
    real :: u(n)
contains
    pure real function f(k) result(g)
        integer, intent(in) :: k
        real :: tmp(2)
        tmp(1) = u(k)
        tmp(2) = u(min(k + 3, n))
        g = 0.5 * (tmp(1) + tmp(2))
    end function
end module

program gpu_metal_531
    use gpu_metal_531_m
    implicit none
    real :: d(n), r(n), e(n)
    integer :: it, k
    do k = 1, n
        u(k) = sin(0.01 * k) + 0.1 * mod(k, 5)
    end do
    r = u
    do it = 1, 6
        do concurrent (k = 1:n)
            d(k) = f(k)
        end do
        do concurrent (k = 1:n)
            u(k) = d(k)
        end do
    end do
    do it = 1, 6
        do k = 1, n
            e(k) = 0.5 * (r(k) + r(min(k + 3, n)))
        end do
        do k = 1, n
            r(k) = e(k)
        end do
    end do
    print *, maxval(abs(u - r))
    if (maxval(abs(u - r)) > 1e-5) error stop 1
    print *, "ok"
end program
