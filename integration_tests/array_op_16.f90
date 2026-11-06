! A reduction nested in an argument of a side-effect free call is evaluated
! once and accumulated in a loop, so its operand is not materialised.
module array_op_16_mod
    use, intrinsic :: iso_fortran_env, only: int16, real32
    implicit none
contains

    pure elemental function widen(bits) result(value)
        integer(int16), intent(in) :: bits
        real(real32) :: value

        value = real(bits, kind=real32)
    end function widen

    pure elemental function halve(value) result(half)
        real(real32), intent(in) :: value
        real(real32) :: half

        half = 0.5_real32*value
    end function halve

    pure subroutine combine(partials, factors, total, out_col, d)
        integer(int16), intent(in) :: partials(:, :)
        real(real32), intent(in) :: factors(:), total
        integer, intent(in) :: d
        real(real32), intent(out) :: out_col

        out_col = halve(sum(widen(partials(d, :))*factors)/total)
    end subroutine combine

end module array_op_16_mod

program array_op_16
    use, intrinsic :: iso_fortran_env, only: int16, real32
    use array_op_16_mod, only: combine
    implicit none
    integer, parameter :: blocks = 6, rows = 3
    integer(int16) :: partials(rows, blocks)
    real(real32) :: factors(blocks), total, expected, got
    integer :: b, r

    do r = 1, rows
        do b = 1, blocks
            partials(r, b) = int(b - r, kind=int16)
        end do
    end do
    do b = 1, blocks
        factors(b) = real(b, kind=real32)/4.0_real32
    end do
    total = 2.0_real32

    do r = 1, rows
        expected = 0.0_real32
        do b = 1, blocks
            expected = expected + real(partials(r, b), kind=real32)*factors(b)
        end do
        expected = 0.5_real32*(expected/total)
        call combine(partials, factors, total, got, r)
        if (got /= expected) error stop "nested call reduction mismatch"
    end do
end program array_op_16
