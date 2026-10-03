program pdt_16
    ! Test PDT structure constructor with default kind parameter.
    ! Both keyword and positional argument forms must work.
    implicit none
    integer, parameter :: sp = kind(1.)

    type :: operands_t(k)
        integer, kind :: k = sp
        real(k) :: actual, expected
    end type

    type(operands_t) :: op1, op2

    ! Keyword arguments (kind parameter uses default)
    op1 = operands_t(actual=1.0, expected=2.0)
    if (abs(op1%actual - 1.0) > 1e-6) error stop
    if (abs(op1%expected - 2.0) > 1e-6) error stop

    ! Positional arguments (kind parameter given explicitly)
    op2 = operands_t(4, 3.0, 4.0)
    if (abs(op2%actual - 3.0) > 1e-6) error stop
    if (abs(op2%expected - 4.0) > 1e-6) error stop

    print *, "ok"
end program
