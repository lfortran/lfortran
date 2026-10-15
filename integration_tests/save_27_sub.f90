subroutine save_27_sub(n)
    implicit none
    integer, intent(in) :: n
    complex, save :: w = (1.0, 2.0)
    complex(8) :: z
    data z /(3.0d0, 4.0d0)/
    if (w /= cmplx(n, 2)) error stop "declaration initialized complex"
    if (z /= cmplx(2 + n, 4, 8)) error stop "DATA initialized complex"
    w = w + 1
    z = z + 1
end subroutine
