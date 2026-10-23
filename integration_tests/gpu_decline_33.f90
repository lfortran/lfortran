! Variables of common blocks that a procedure called in a do concurrent loop
! names, which the gpu kernel cannot be handed: one that the declaration of a
! dummy names, and a character variable that a procedure handed over as an
! actual argument reads. The gpu offload reports each loop as an error (see
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

    pure real function from_character(k)
        integer, intent(in) :: k
        character(len=4) :: c
        common /gpu_decline_33_c/ c
        from_character = len_trim(c) + k
    end function

    pure real function apply(f, k)
        interface
            pure real function f(k)
                integer, intent(in) :: k
            end function
        end interface
        integer, intent(in) :: k
        apply = f(k)
    end function
end module

program gpu_decline_33
    use gpu_decline_33_mod
    implicit none
    integer :: m
    common /gpu_decline_33_n/ m
    character(len=4) :: c
    common /gpu_decline_33_c/ c
    real :: a(4), u(4)
    integer :: k

    m = 4
    a = [1, 2, 3, 4]
    do concurrent (k = 1:4)
        u(k) = last_of(a, k)
    end do
    if (any(u /= [4, 8, 12, 16])) error stop 1

    c = "ab"
    do concurrent (k = 1:4)
        u(k) = apply(from_character, k)
    end do
    if (any(u /= [3, 4, 5, 6])) error stop 2
    print *, u
end program
