program real128_arith_11
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128), parameter :: p = 3.0_real128 / 2.0_real128
    real(real128) :: a, b

    if (p /= 1.5_real128) error stop 1
    if (7.0_real128 / 8.0_real128 /= 0.875_real128) error stop 2

    a = 3.0_real128
    b = 2.0_real128
    call check(a / b, 1.5_real128, 3)
    b = 1.0_real128
    call check(a / b, 3.0_real128, 4)
    a = 9.0_real128
    b = 4.0_real128
    call check(a / b, 2.25_real128, 5)
    a = 1.0_real128
    b = 3.0_real128
    call check(a / b * 3.0_real128, 1.0_real128, 6)
    a = 4.0_real128
    call check(sqrt(a), 2.0_real128, 7)
    print *, p

contains

    subroutine check(x, expected, code)
        real(real128), intent(in) :: x, expected
        integer, intent(in) :: code
        if (x /= expected) error stop code
    end subroutine

end program
