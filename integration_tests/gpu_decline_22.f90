! Variables of common blocks that procedures called in an OpenMP loop
! offloaded to the gpu name, which the gpu kernel cannot be handed: a scalar
! a procedure defines, a variable an external procedure reads, and a block
! one procedure defines that another lays out differently. The gpu offload
! reports each loop as an error (see tests/tests.toml); without it the
! program runs on the host.
module gpu_decline_22_mod
    implicit none
contains
    subroutine record(k)
        integer, intent(in) :: k
        integer :: last
        common /gpu_decline_22_l/ last
        if (k == 4) last = k
    end subroutine

    subroutine put(k)
        integer, intent(in) :: k
        real :: pad(3), w(4)
        common /gpu_decline_22_w/ pad, w
        w(k) = 10 * k
    end subroutine

    real function get(k)
        integer, intent(in) :: k
        real :: a(7)
        common /gpu_decline_22_w/ a
        get = a(k + 3)
    end function
end module

subroutine gpu_decline_22_add(k, v)
    integer, intent(in) :: k
    integer, intent(out) :: v
    integer :: base
    common /gpu_decline_22_b/ base
    v = base + k
end subroutine

program gpu_decline_22
    use gpu_decline_22_mod
    implicit none
    integer :: last, base, k, n(4)
    real :: u(4)
    common /gpu_decline_22_l/ last
    common /gpu_decline_22_b/ base

    last = -1
    !$omp parallel do
    do k = 1, 4
        call record(k)
    end do
    !$omp end parallel do
    if (last /= 4) error stop 1

    base = 10
    !$omp parallel do
    do k = 1, 4
        call gpu_decline_22_add(k, n(k))
    end do
    !$omp end parallel do
    if (any(n /= [11, 12, 13, 14])) error stop 2

    !$omp parallel do
    do k = 1, 4
        call put(k)
        u(k) = get(k)
    end do
    !$omp end parallel do
    if (any(u /= [10, 20, 30, 40])) error stop 3
    print *, last, n, u
end program
