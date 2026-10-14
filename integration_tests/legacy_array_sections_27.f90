program legacy_array_sections_27
    implicit none
    real :: a(3, 3)
    a = 0.0
    call s(a, 3, 3)
    print *, a
    if (any(a(:, 1:2) /= 0.0)) error stop
    if (a(1, 3) /= 0.0) error stop
    if (abs(a(2, 3) - 3.0) > 1e-6) error stop
    if (abs(a(3, 3) - 3.0) > 1e-6) error stop
end program

subroutine s(a, lda, j)
    implicit none
    integer, intent(in) :: lda, j
    real :: a(lda, 1)
    call add1(2, a(2, j))
    call add2(2, a(2, j))
end subroutine

subroutine add1(n, y)
    implicit none
    integer, intent(in) :: n
    real :: y(n)
    y = y + 1.0
end subroutine
