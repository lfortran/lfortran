! Variables of common blocks that a procedure called in a do concurrent loop
! reads, which the gpu kernel cannot be handed: a pointer, a character
! variable, and a block that places the variable read after a derived-type
! variable. The gpu offload reports each loop as an error (see
! tests/tests.toml); without it the program runs on the host.
module gpu_decline_24_mod
    implicit none
    type :: pair_t
        sequence
        real :: a, b
    end type
contains
    pure real function from_pointer(k)
        integer, intent(in) :: k
        real, pointer :: p
        common /gpu_decline_24_p/ p
        from_pointer = p + k
    end function

    pure real function from_character(k)
        integer, intent(in) :: k
        character(len=4) :: c
        common /gpu_decline_24_c/ c
        from_character = len_trim(c) + k
    end function

    pure real function after_derived_type(k)
        integer, intent(in) :: k
        type(pair_t) :: s
        real :: x
        common /gpu_decline_24_s/ s, x
        after_derived_type = x + k
    end function
end module

program gpu_decline_24
    use gpu_decline_24_mod
    implicit none
    real, target :: t
    real, pointer :: p
    character(len=4) :: c
    type(pair_t) :: s
    real :: x, u(4)
    common /gpu_decline_24_p/ p
    common /gpu_decline_24_c/ c
    common /gpu_decline_24_s/ s, x
    integer :: k

    t = 10
    p => t
    do concurrent (k = 1:4)
        u(k) = from_pointer(k)
    end do
    if (any(u /= [11, 12, 13, 14])) error stop 1

    c = "ab"
    do concurrent (k = 1:4)
        u(k) = from_character(k)
    end do
    if (any(u /= [3, 4, 5, 6])) error stop 2

    s = pair_t(1, 2)
    x = 20
    do concurrent (k = 1:4)
        u(k) = after_derived_type(k)
    end do
    if (any(u /= [21, 22, 23, 24])) error stop 3
    print *, u
end program
