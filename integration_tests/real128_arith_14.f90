program real128_arith_14
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128), parameter :: one = 1.0_real128
    real(real128), parameter :: eps = epsilon(one)
    real(real128), parameter :: p = 2.5_real128 + (0.0_real128 - 26.5_real128)
    real(real128) :: a, b

    if (p /= 0.0_real128 - 24.0_real128) error stop 1
    if (one + 0.75_real128 * eps /= one + eps) error stop 2

    a = 9.0_real128
    b = 18.0_real128
    call check(a - b, 0.0_real128 - 9.0_real128, 3)
    call check(b - a, 9.0_real128, 4)
    call check((a - b) + b, a, 5)
    a = one
    b = 0.75_real128 * eps
    call check(a + b, one + eps, 6)
    b = 0.625_real128 * eps
    call check(a - b, one - eps / 2, 7)
    print *, p

contains

    subroutine check(x, expected, code)
        real(real128), intent(in) :: x, expected
        integer, intent(in) :: code
        if (x /= expected) error stop code
    end subroutine

end program
