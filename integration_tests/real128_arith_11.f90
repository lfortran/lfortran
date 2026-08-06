program real128_arith_11
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) :: a, b, q, eps
    integer :: n, i

    ! Build the values from a quantity that is only known at run time so that
    ! the arithmetic below is really executed instead of being folded away.
    n = command_argument_count()

    ! Division where the dividend mantissa is >= the divisor mantissa.
    a = 3.0_real128 + real(n, real128)
    b = 2.0_real128 + real(n, real128)
    q = a / b
    if (abs(q - 1.5_real128) > 1.0e-30_real128) error stop 1

    ! Division where the dividend mantissa is < the divisor mantissa.
    q = 1.0_real128 / (3.0_real128 + real(n, real128))
    if (abs(q * 3.0_real128 - 1.0_real128) > 1.0e-30_real128) error stop 2

    ! Exact division.
    a = 2.0_real128 + real(n, real128)
    q = a / a
    if (q /= 1.0_real128) error stop 3

    ! Conversion to double/single of a value whose 113-bit mantissa is all
    ! ones (1 - 2^-113): rounding must carry into the exponent and give
    ! exactly 1.0, not a value with a stray fraction bit.
    eps = 1.0_real128 + real(n, real128)
    do i = 1, 113
        eps = eps * 0.5_real128
    end do
    a = 1.0_real128 - eps
    if (real(a, 8) /= 1.0d0) error stop 4
    if (real(a, 4) /= 1.0) error stop 5

    print *, "ok"
end program real128_arith_11
