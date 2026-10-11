program expr_22
    ! Parentheses that group the right operand of a binary operator
    ! of the same precedence change the result
    implicit none
    real(8) :: a, b, c, x
    integer :: i, j, k
    logical :: l, m, n
    a = 1.0d20
    b = -1.0d20
    c = 1.0d0
    x = a + (b + c)
    if (x /= 0.0d0) error stop
    x = a + b + c
    if (x /= 1.0d0) error stop
    a = huge(a)
    b = 2.0d0
    c = 0.5d0
    x = a*(b*c)
    if (x /= a) error stop
    x = -(-a)
    if (x /= a) error stop
    i = 2
    j = 3
    k = 2
    if ((i**j)**k /= 64) error stop
    if (i**j**k /= 512) error stop
    if (i - (j - k) /= 1) error stop
    l = .true.
    m = .true.
    n = .true.
    if (.not. (l .or. (m .neqv. n))) error stop
    if ((l .or. m) .neqv. n) error stop
    if (.not. (l .and. (m .and. n))) error stop
end program expr_22
