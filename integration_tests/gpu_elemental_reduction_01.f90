! A reduction over an expression built from a pure elemental function of an
! array section is accumulated in one loop, so the function is applied to one
! element at a time and neither its result nor the product is evaluated into
! an array temporary. The values are exact in real(4), so the checks do not
! depend on the accumulation order.
module elemental_reduction
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

    pure subroutine attention_rows(k, q, s, nested, largest, above, half_sum)
        integer(int16), contiguous, intent(in) :: k(:, :)
        real(real32), contiguous, intent(in) :: q(:)
        real(real32), contiguous, intent(out) :: s(:), nested(:)
        real(real32), intent(out) :: largest, half_sum
        integer, contiguous, intent(out) :: above(:)
        integer :: t

        if (size(k, 1) /= size(q) .or. size(s) /= size(k, 2) .or. &
                size(above) /= size(k, 2) .or. size(nested) /= size(k, 2)) &
            error stop "inconsistent shapes"
        do concurrent (integer :: t = 1:size(k, 2))
            s(t) = sum(q*widen(k(:, t)))
            nested(t) = halve(sum(q*widen(k(:, t))))
            above(t) = count(widen(k(:, t)) > 1.0_real32)
        end do
        largest = maxval(halve(q)*widen(k(:, 1)))
        half_sum = sum(halve(q))
    end subroutine attention_rows

end module elemental_reduction

program gpu_elemental_reduction_01
    use elemental_reduction, only: attention_rows
    use, intrinsic :: iso_fortran_env, only: int16, real32
    implicit none
    integer, parameter :: rows = 6, width = 8
    integer(int16) :: k(width, rows)
    real(real32) :: q(width), s(rows), nested(rows), largest, half_sum, expected
    integer :: above(rows), i, t, expected_above

    do t = 1, rows
        do i = 1, width
            k(i, t) = int(mod(i + 2*t, 7) - 3, kind=int16)
        end do
    end do
    do i = 1, width
        q(i) = real(i, kind=real32)
    end do

    call attention_rows(k, q, s, nested, largest, above, half_sum)

    do t = 1, rows
        expected = 0.0_real32
        expected_above = 0
        do i = 1, width
            expected = expected + q(i)*real(k(i, t), kind=real32)
            if (real(k(i, t), kind=real32) > 1.0_real32) expected_above = expected_above + 1
        end do
        if (s(t) /= expected) error stop "sum of the elemental product"
        if (nested(t) /= 0.5_real32*expected) error stop "nested elemental product"
        if (above(t) /= expected_above) error stop "count of the widened row"
    end do

    expected = 0.0_real32
    do i = 1, width
        expected = max(expected, 0.5_real32*q(i)*real(k(i, 1), kind=real32))
    end do
    if (largest /= expected) error stop "maxval of the elemental product"

    expected = 0.0_real32
    do i = 1, width
        expected = expected + 0.5_real32*q(i)
    end do
    if (half_sum /= expected) error stop "sum of the elemental function"
end program gpu_elemental_reduction_01
