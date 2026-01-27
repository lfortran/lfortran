program mre
    implicit none

    type :: line_token
        integer :: first, second
    end type

    type(line_token), allocatable :: token(:)
    integer :: shift(4)

    allocate(token(4))
    token%first = [1, 2, 3, 4]

    associate(first => token%first)
        shift = first - 1
    end associate

    print *, shift
end program mre
