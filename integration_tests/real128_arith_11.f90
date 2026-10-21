program real128_arith_11
    ! Runtime real(128) division must be correctly rounded, also when the
    ! dividend's significand is larger than the divisor's and for results in
    ! the subnormal and overflow ranges.
    use, intrinsic :: iso_fortran_env, only: real128, int64
    implicit none
    real(real128) :: a, b, q, zero
    integer :: n

    ! Zero known only at run time, so that no division is constant-folded.
    n = command_argument_count()
    zero = n

    a = 3 + zero
    b = 2 + zero
    q = a / b
    print *, q
    if (q /= 1.5_real128) error stop 1

    a = 1.5_real128 + zero
    b = 1.25_real128 + zero
    q = a / b
    print *, q
    call check(q, [3689348814741910323_int64, 4611460838446019379_int64], 2)

    a = 7 + zero
    b = 3 + zero
    q = a / b
    print *, q
    call check(q, [-6148914691236517205_int64, 4611732930923506346_int64], 3)

    a = -9 + zero
    b = 4 + zero
    q = a / b
    print *, q
    call check(q, [0_int64, -4611650834055299072_int64], 4)

    a = 3 + zero
    b = 7 + zero
    q = a / b
    print *, q
    call check(q, [-5270498306774157605_int64, 4611042647052049261_int64], 5)

    a = 1 + zero
    b = 3 + zero
    q = a / b
    call check(q, [6148914691236517205_int64, 4610935418489492821_int64], 6)

    ! Subnormal results: exact, halfway cases rounded to even, and a carry
    ! into the smallest normal number. Each division uses its own divisor.
    a = tiny(a) + zero
    b = 8 + zero
    q = a / b
    call check(q, [0_int64, 35184372088832_int64], 7)

    a = transfer([40_int64 + n, 0_int64], a)
    b = 16 + zero
    q = a / b
    call check(q, [2_int64, 0_int64], 8)

    a = transfer([112_int64 + n, 0_int64], a)
    b = 32 + zero
    q = a / b
    call check(q, [4_int64, 0_int64], 9)

    a = transfer([-1_int64, 562949953421311_int64 + n], a)
    b = 2 + zero
    q = a / b
    call check(q, [0_int64, 281474976710656_int64], 10)

    ! Overflow to infinity and underflow to zero.
    a = huge(a) + zero
    b = 0.5_real128 + zero
    q = a / b
    call check(q, [0_int64, 9223090561878065152_int64], 11)

    a = tiny(a) + zero
    b = huge(b) + zero
    q = a / b
    call check(q, [0_int64, 0_int64], 12)

contains

    subroutine check(x, expected, code)
        real(real128), intent(in) :: x
        integer(int64), intent(in) :: expected(2)
        integer, intent(in) :: code
        integer(int64) :: bits(2)
        bits = transfer(x, bits)
        if (any(bits /= expected)) then
            print *, code, bits
            error stop code
        end if
    end subroutine

end program
