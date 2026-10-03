program expr_25
    ! Parentheses that the grouping does not need are kept in the AST,
    ! so that --show-ast-f90 prints them back: the standard requires
    ! them to be honored, and compilers do
    implicit none
    real :: a, b, c, x
    a = 1.0
    b = 2.0
    c = 3.0
    x = (a*b) + c
    if (x /= 5.0) error stop
    x = (a/b)*c
    if (x /= 1.5) error stop
    x = ((a + b))
    if (x /= 3.0) error stop
    x = c - (a*b)
    if (x /= 1.0) error stop
    if ((a + b) > c) error stop
end program expr_25
