program real128_intrinsic_math_02
    ! The pi variants of the trigonometric intrinsics of real(128)
    ! arguments known only at run time must be computed in quad precision.
    ! Not labelled gfortran: GFortran accepts sinpi etc. for real(128), but
    ! its libquadmath has no sinpiq, so the program does not link.
    use, intrinsic :: iso_fortran_env, only: real128, int64
    implicit none
    integer, parameter :: qp = real128
    real(qp) :: zero, one, two, half, x
    integer :: n

    ! Zero known only at run time, so that nothing is constant-folded.
    ! max(..., 0): on wasm32-wasi, LFortran's entry point passes argc = 0,
    ! so command_argument_count() returns -1 there.
    n = max(command_argument_count(), 0)
    zero = n
    one = 1 + zero
    two = 2 + zero
    half = one / 2

    call check(sinpi(half), one, 1)
    call check(sinpi(one / 6), half, 2)
    call check(sinpi(zero - 1.5_qp), one, 3)
    call check(cospi(one / 3), half, 4)
    call check(cospi(two), one, 5)
    call check(-cospi(one), one, 6)
    call check(tanpi(one / 4), one, 7)
    call check(-tanpi(-one / 4), one, 8)
    call check(asinpi(half), one / 6, 9)
    call check(-asinpi(-one), half, 10)
    call check(acospi(half), one / 3, 11)
    call check(acospi(-one), one, 12)
    call check(atanpi(one), one / 4, 13)
    call check(atan2pi(one, -one), 3 / (4 * one), 14)
    call check(-atan2pi(-one, -one), 3 / (4 * one), 15)
    ! sin(pi) with pi rounded to binary128 is about 8.7e-35; a pi rounded
    ! to binary64 would give about 1.2e-16.
    if (.not. (abs(sinpi(one)) <= 1e-33_qp)) error stop 16
    if (.not. (abs(cospi(half)) <= 1e-33_qp)) error stop 17

    ! (-1)**y is 1 for every even y, also for |y| > 2**16000. Raising a
    ! negative real to a real power is not standard Fortran (processor
    ! dependent); this checks the runtime's C99 pow semantics for |x| == 1.
    x = transfer([0_int64, 9115285645797883904_int64 + n], x)   ! 2**16001
    call check((zero - 1) ** x, one, 40)
    call check((zero - 1) ** (-x), one, 41)
    call check((zero - 1) ** huge(x), one, 42)

contains

    subroutine check(x, ref, code)
        real(qp), intent(in) :: x, ref
        integer, intent(in) :: code
        if (.not. (abs(x - ref) <= 4 * epsilon(ref) * abs(ref))) then
            print *, code, x, ref
            error stop code
        end if
    end subroutine

end program
