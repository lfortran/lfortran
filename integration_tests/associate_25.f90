program associate_23
    implicit none

    type :: line_token
        integer :: first
    end type

    type(line_token), allocatable :: token(:)
    integer :: shift

    allocate(token(2))
    token%first = [2, 4]

    associate(first => token%first)
        shift = first(1) - 1
    end associate

    if (shift /= 1) error stop "shift mismatch"
end program associate_23
