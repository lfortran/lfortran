program real128_intrinsic_math_03
    ! The exp2 extension of real(128) arguments known only at run time must
    ! be computed in quad precision. exp2 is an LFortran extension, not a
    ! Fortran or GFortran intrinsic, so this test is not labelled gfortran
    ! and does not run with --std=f23, where exp2 names an external
    ! procedure.
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    integer, parameter :: qp = real128
    real(qp) :: zero, one, two, half
    integer :: i

    ! Zero known only at run time, so that nothing is constant-folded.
    ! max(..., 0): on wasm32-wasi, LFortran's entry point passes argc = 0,
    ! so command_argument_count() returns -1 there.
    zero = max(command_argument_count(), 0)
    one = 1 + zero
    two = 2 + zero
    half = one / 2

    call check(exp2(half), sqrt(two), 1)
    call check(exp2(zero + 10), 1024 + zero, 2)
    call check(exp2(zero - 3), one / 8, 3)
    call check(exp2(zero + 100.25_qp), pow2(100) * sqrt(sqrt(two)), 4)
    call check(exp2(zero - 1000.75_qp), pow2(-1001) * sqrt(sqrt(two)), 5)
    do i = -16, 16
        call check(exp2(zero + i), pow2(i), 10)
    end do

contains

    ! 2**n, computed exactly
    function pow2(n) result(r)
        integer, intent(in) :: n
        real(qp) :: r
        integer :: k
        r = 1
        do k = 1, abs(n)
            if (n > 0) then
                r = r * 2
            else
                r = r / 2
            end if
        end do
    end function

    subroutine check(x, ref, code)
        real(qp), intent(in) :: x, ref
        integer, intent(in) :: code
        if (.not. (abs(x - ref) <= 4 * epsilon(ref) * abs(ref))) then
            print *, code, x, ref
            error stop code
        end if
    end subroutine

end program
