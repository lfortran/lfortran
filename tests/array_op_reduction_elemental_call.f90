! A reduction with a scalar result over an expression that references a pure
! elemental function of an array section is accumulated in one loop over the
! elements of the expression: the function is applied to one element at a
! time, so neither its result nor the expression is evaluated into an
! allocatable array temporary first.
module array_op_reduction_elemental_call_mod
implicit none
contains

    pure elemental function widen(x) result(y)
        integer(2), intent(in) :: x
        real(4) :: y

        y = real(x, kind=4)
    end function widen

    pure elemental function halve(x) result(y)
        real(4), intent(in) :: x
        real(4) :: y

        y = 0.5_4*x
    end function halve

end module array_op_reduction_elemental_call_mod

subroutine array_op_reduction_elemental_call(k, q, s, m, c, d, h)
    use array_op_reduction_elemental_call_mod, only: widen, halve
    implicit none
    integer(2), intent(in) :: k(:, :)
    real(4), intent(in) :: q(:)
    real(4), intent(out) :: s(:), m, d, h(:)
    integer, intent(out) :: c(:)
    integer :: t

    do t = 1, size(k, 2)
        s(t) = sum(q*widen(k(:, t)))
    end do
    m = maxval(halve(q)*widen(k(:, 1)))
    do t = 1, size(k, 2)
        c(t) = count(widen(k(:, t)) > 4.0_4)
    end do
    d = sum(halve(q))
    ! A reduction nested in a pure scalar call is evaluated into a scalar
    ! first, so the enclosing call reads it once.
    do t = 1, size(k, 2)
        h(t) = halve(sum(q*widen(k(:, t))))
    end do
end subroutine array_op_reduction_elemental_call
