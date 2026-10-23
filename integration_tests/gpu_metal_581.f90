! Procedures called in an offloaded OpenMP loop define elements of an array
! of a common block: the host sees them after the loop, and another
! procedure with the same layout of the block, and the loop itself, read
! them in the same iteration.
module gpu_metal_581_mod
    implicit none
contains
    subroutine put(k)
        integer, intent(in) :: k
        real :: w(4)
        common /gpu_metal_581_c/ w
        w(k) = 10 * k
    end subroutine

    subroutine put_shared(k)
        integer, intent(in) :: k
        real :: w(4)
        common /gpu_metal_581_s/ w
        w(k) = 10 * k
    end subroutine

    real function get_shared(k)
        integer, intent(in) :: k
        real :: x(4)
        common /gpu_metal_581_s/ x
        get_shared = x(k) + 1
    end function
end module

program gpu_metal_581
    use gpu_metal_581_mod
    implicit none
    real :: d(4), e(4), u(4), t(4)
    common /gpu_metal_581_c/ d
    common /gpu_metal_581_s/ e
    integer :: k

    d = -1
    !$omp parallel do
    do k = 1, 4
        call put(k)
    end do
    !$omp end parallel do
    print *, d
    if (any(d /= [10, 20, 30, 40])) error stop 1

    e = -1
    !$omp parallel do
    do k = 1, 4
        call put_shared(k)
        u(k) = get_shared(k)
        t(k) = e(k)
    end do
    !$omp end parallel do
    print *, e, u, t
    if (any(e /= [10, 20, 30, 40])) error stop 2
    if (any(u /= [11, 21, 31, 41])) error stop 3
    if (any(t /= [10, 20, 30, 40])) error stop 4
end program
