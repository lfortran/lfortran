! A procedure called in an offloaded OpenMP loop reads a member of a common
! block that EQUIVALENCE associates another variable with: the member is the
! storage of the block on the device as well.
module gpu_metal_569_mod
    implicit none
contains
    real function member_read(k)
        integer, intent(in) :: k
        real :: w(4), e(2)
        common /gpu_metal_569_c/ w
        equivalence (w(3), e)
        member_read = w(k) * k
    end function
end module

program gpu_metal_569
    use gpu_metal_569_mod
    implicit none
    real :: d(4), u(4)
    common /gpu_metal_569_c/ d
    integer :: k
    d = [1, 2, 3, 4]
    !$omp parallel do
    do k = 1, 4
        u(k) = member_read(k)
    end do
    !$omp end parallel do
    print *, u
    if (any(u /= [1, 4, 9, 16])) error stop
end program
