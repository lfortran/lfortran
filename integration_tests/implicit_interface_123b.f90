! Solves a(1:n, 1:n) x = b for diagonally dominant a, no pivoting.
subroutine implicit_interface_123_solve(n, a, lda, b, info)
    integer :: n, lda, info, i, k
    real(8) :: a(lda, *), b(*), f
    info = 0
    do k = 1, n - 1
        do i = k + 1, n
            f = a(i, k)/a(k, k)
            a(i, k + 1:n) = a(i, k + 1:n) - f*a(k, k + 1:n)
            b(i) = b(i) - f*b(k)
        end do
    end do
    do i = n, 1, -1
        b(i) = (b(i) - sum(a(i, i + 1:n)*b(i + 1:n)))/a(i, i)
    end do
end subroutine

real(8) function implicit_interface_123_dot(n, x, y)
    integer :: n
    real(8) :: x(*), y(*)
    implicit_interface_123_dot = sum(x(1:n)*y(1:n))
end function

subroutine implicit_interface_123_scale(n, x, s)
    integer :: n
    real(8) :: x(*), s
    x(1:n) = s*x(1:n)
end subroutine
