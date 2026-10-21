program real128_arith_12
    ! Runtime real(128) addition and subtraction must be correctly rounded
    ! (to nearest, ties to even), also when an operand is shifted out
    ! entirely and when the difference of close numbers is exact.
    use, intrinsic :: iso_fortran_env, only: real128, int64
    implicit none
    real(real128) :: one, a, b, c, zero
    integer :: n

    ! Zero known only at run time, so that nothing is constant-folded.
    n = command_argument_count()
    zero = n
    one = 1 + zero

    ! 1 + 2**-112 minus 1 - 2**-113: exact result 1.5 * 2**-112.
    a = transfer([1_int64 + n, 4611404543450677248_int64], a)
    b = transfer([-1_int64, 4611404543450677247_int64 + n], b)
    c = a - b
    call check(c, [0_int64, 4580020083547439104_int64], 1)

    ! 1 + 3 * 2**-114 rounds up to 1 + 2**-112.
    b = transfer([0_int64, 4579738608570728448_int64 + n], b)
    c = one + b
    call check(c, [1_int64, 4611404543450677248_int64], 2)

    ! 1 - 3 * 2**-114 rounds to 1 - 2 * 2**-113.
    c = one - b
    call check(c, [-2_int64, 4611404543450677247_int64], 3)

    ! Ties round to even: 1 + 2**-113 gives 1, (1 + 2**-112) + 2**-113
    ! gives 1 + 2**-111.
    b = transfer([0_int64, 4579597871082373120_int64 + n], b)
    c = one + b
    call check(c, [0_int64, 4611404543450677248_int64], 4)
    c = a + b
    call check(c, [2_int64, 4611404543450677248_int64], 5)

    ! An addend far below the last bit leaves 1 unchanged.
    b = transfer([0_int64, 4555109548108546048_int64 + n], b)
    c = one + b
    call check(c, [0_int64, 4611404543450677248_int64], 6)
    c = one - b
    call check(c, [0_int64, 4611404543450677248_int64], 7)

    ! x - x is +0.
    c = a - a
    call check(c, [0_int64, 0_int64], 8)

    ! 2 - 3 * 2**-114 rounds to 2 - 2**-112.
    a = 2 + zero
    b = transfer([0_int64, 4579738608570728448_int64 + n], b)
    c = a - b
    call check(c, [-1_int64, 4611686018427387903_int64], 9)

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
