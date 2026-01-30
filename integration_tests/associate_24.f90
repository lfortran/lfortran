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
        first = first + 1
    end associate

    ! Check that shift = first - 1 worked (original values were 1,2,3,4)
    if (shift(1) /= 0) error stop
    if (shift(2) /= 1) error stop
    if (shift(3) /= 2) error stop
    if (shift(4) /= 3) error stop

    ! Check that first = first + 1 modified the original array
    if (token(1)%first /= 2) error stop
    if (token(2)%first /= 3) error stop
    if (token(3)%first /= 4) error stop
    if (token(4)%first /= 5) error stop

    print *, "PASSED"
end program mre
