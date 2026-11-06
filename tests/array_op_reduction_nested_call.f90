! A reduction nested in an argument of a side-effect free call is evaluated
! once, before the statement, like one in an intrinsic operation, so the
! call argument is not evaluated into an array temporary.
module array_op_reduction_nested_call_mod
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

end module array_op_reduction_nested_call_mod

subroutine array_op_reduction_nested_call(partials, factors, total, out_col, d)
    use array_op_reduction_nested_call_mod, only: widen, halve
    implicit none
    integer(2), intent(in) :: partials(:, :)
    real(4), intent(in) :: factors(:), total
    integer, intent(in) :: d
    real(4), intent(out) :: out_col

    out_col = halve(sum(widen(partials(d, :))*factors)/total)
end subroutine array_op_reduction_nested_call
