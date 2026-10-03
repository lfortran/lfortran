subroutine test_sub(x, n, y)
    integer, intent(in) :: n
    real, intent(in) :: x(n, n)
    real, intent(out) :: y(n, n)
    y = reshape(reshape(x, [n*n]), [n, n])
end subroutine

program main
    implicit none
    real :: x(2,2), y(2,2)
    x = reshape([1.0, 2.0, 3.0, 4.0], [2, 2])
    call test_sub(x, 2, y)
    if (any(y /= x)) error stop
    print *, "OK"
end program