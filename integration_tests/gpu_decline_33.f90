! Variables of common blocks that a procedure called in a do concurrent loop
! names, which the gpu kernel cannot be handed: one that the declaration of a
! dummy names. The gpu offload reports each loop as an error (see
! tests/tests.toml); without it the program runs on the host.
module gpu_decline_33_mod
    implicit none
contains
    ! The extent of an explicit-shape dummy is a variable of a block.
    pure real function last_of(x, k)
        integer, intent(in) :: k
        integer :: nn
        common /gpu_decline_33_n/ nn
        real, intent(in) :: x(nn)
        last_of = x(nn) * k
    end function
end module

program gpu_decline_33
    use gpu_decline_33_mod
    implicit none
    integer :: m
    common /gpu_decline_33_n/ m
    real :: a(4), u(4)
    integer :: k

    m = 4
    a = [1, 2, 3, 4]
    do concurrent (k = 1:4)
        u(k) = last_of(a, k)
    end do
    if (any(u /= [4, 8, 12, 16])) error stop 1
    print *, u
end program
