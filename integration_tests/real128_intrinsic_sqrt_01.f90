program real128_intrinsic_sqrt_01
    ! Runtime real(128) sqrt must be correctly rounded, also for subnormal
    ! arguments, and must handle signed zeros, infinity and negative values.
    use, intrinsic :: iso_fortran_env, only: real128, int64
    implicit none
    real(real128) :: x, s, zero
    integer(int64) :: bits(2)
    integer :: n

    ! Zero known only at run time, so that no sqrt is constant-folded.
    n = command_argument_count()
    zero = n

    x = 2 + zero
    s = sqrt(x)
    print *, s
    call check(s, [-3960719080840303979_int64, 4611521134203499452_int64], 1)

    x = 3 + zero
    s = sqrt(x)
    call check(s, [-6396477664840877896_int64, 4611610597434688714_int64], 2)

    x = 10 + zero
    s = sqrt(x)
    call check(s, [2963450106712477084_int64, 4611849594466051509_int64], 3)

    x = 0.5_real128 + zero
    s = sqrt(x)
    call check(s, [-3960719080840303979_int64, 4611239659226788796_int64], 4)

    x = 9 + zero
    s = sqrt(x)
    if (s /= 3) error stop 5

    x = huge(x) + zero
    s = sqrt(x)
    call check(s, [-1_int64, 6917247552664371199_int64], 6)

    x = tiny(x) + zero
    s = sqrt(x)
    call check(s, [0_int64, 2305843009213693952_int64], 7)

    ! Subnormal arguments: the smallest one, tiny/2 and a generic one.
    x = transfer([1_int64 + n, 0_int64], x)
    s = sqrt(x)
    call check(s, [0_int64, 2290080410517897216_int64], 8)

    x = transfer([0_int64 + n, 140737488355328_int64], x)
    s = sqrt(x)
    call check(s, [-3960719080840303979_int64, 2305678124989805500_int64], 9)

    x = transfer([123456789_int64 + n, 1234_int64], x)
    s = sqrt(x)
    call check(s, [3638412220841500579_int64, 2300522501791888067_int64], 10)

    ! sqrt(+0) = +0, sqrt(-0) = -0, sqrt(+inf) = +inf.
    x = zero
    s = sqrt(x)
    call check(s, [0_int64, 0_int64], 11)

    x = transfer([0_int64 + n, -huge(0_int64) - 1], x)
    s = sqrt(x)
    call check(s, [0_int64, -huge(0_int64) - 1], 12)

    x = transfer([0_int64 + n, 9223090561878065152_int64], x)
    s = sqrt(x)
    call check(s, [0_int64, 9223090561878065152_int64], 13)

    ! A negative argument gives a NaN.
    x = -4 + zero
    s = sqrt(x)
    bits = transfer(s, bits)
    if (iand(bits(2), 9223090561878065152_int64) /= 9223090561878065152_int64) &
        error stop 14
    if (iand(bits(2), 281474976710655_int64) == 0 .and. bits(1) == 0) &
        error stop 15

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
