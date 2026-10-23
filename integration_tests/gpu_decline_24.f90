! Variables of common blocks that a procedure called in a do concurrent loop
! reads, which the gpu kernel cannot be handed: a pointer, a character
! variable, a block that places the variable read after a derived-type
! variable, a block the loop defines that the procedure lays out
! differently, and a scalar the loop defines, by assignment or as the index
! of a DO loop, or through a variable EQUIVALENCE associates with the block,
! that the procedure reads. The gpu offload reports each loop as an error
! (see tests/tests.toml); without it the program runs on the host.
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

    ! Lays out the block differently from the loop that defines it.
    pure real function laid_out_apart(k)
        integer, intent(in) :: k
        real :: pad(3), w(4)
        common /gpu_decline_24_w/ pad, w
        laid_out_apart = w(k)
    end function

    ! Reads the scalar the loop defines in each iteration, through a view
    ! of its own.
    pure real function defined_by_loop()
        real :: y
        common /gpu_decline_24_y/ y
        defined_by_loop = y
    end function

    ! Reads the scalar the loop defines as the index of a DO loop.
    pure integer function index_by_loop()
        integer :: i
        common /gpu_decline_24_i/ i
        index_by_loop = i
    end function

    ! Reads the block the loop defines through a variable that EQUIVALENCE
    ! associates with it.
    pure integer function aliased_by_loop()
        integer :: v
        common /gpu_decline_24_e/ v
        aliased_by_loop = v
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
    real :: a(7)
    common /gpu_decline_24_w/ a
    real :: y
    common /gpu_decline_24_y/ y
    integer :: i, n(4)
    common /gpu_decline_24_i/ i
    integer :: v, e
    common /gpu_decline_24_e/ v
    equivalence (e, v)
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

    a = 0
    do concurrent (k = 1:4)
        a(k + 3) = k
        u(k) = laid_out_apart(k)
    end do
    if (any(u /= [1, 2, 3, 4])) error stop 4

    y = -1
    do concurrent (k = 1:4)
        y = 10 * k
        u(k) = defined_by_loop()
    end do
    if (any(u /= [10, 20, 30, 40])) error stop 5

    i = -1
    do concurrent (k = 1:4)
        do i = 10 * k, 100
            exit
        end do
        n(k) = index_by_loop()
    end do
    if (any(n /= [10, 20, 30, 40])) error stop 6

    v = -1
    do concurrent (k = 1:4)
        e = 10 * k
        n(k) = aliased_by_loop()
    end do
    if (any(n /= [10, 20, 30, 40])) error stop 7
    print *, u
end program
