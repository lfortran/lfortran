module array_op_reduction_nested_call_mod
implicit none

contains

elemental real(4) function halve(x) result(y)
    real(4), intent(in) :: x
    y = 5.00000000e-01*x
end function halve

elemental real(4) function widen(x) result(y)
    integer(2), intent(in) :: x
    y = Real(x)
end function widen

end module array_op_reduction_nested_call_mod

subroutine array_op_reduction_nested_call(partials, factors, total, out_col, d)
    use array_op_reduction_nested_call_mod, only: halve
    use array_op_reduction_nested_call_mod, only: widen
    integer(2), dimension(:), pointer :: __libasr_created__function_call_widen
    real(4) :: __libasr_created__intrinsic_array_function_Sum
    integer(4) :: __libasr_created__result_metadata_
    integer(4) :: __libasr_created__result_metadata_1
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4) :: __libasr_reduction_acc
    integer(4), intent(in) :: d
    real(4), dimension(:), intent(in) :: factors
    real(4), intent(out) :: out_col
    integer(2), dimension(:, :), intent(in) :: partials
    real(4), intent(in) :: total
    __libasr_created__result_metadata_ = (0 > (ubound(partials, 2) - lbound(partials, 2) + 1)/1 ? 0 : (ubound(partials,&
         2) - lbound(partials, 2) + 1)/1)
    __libasr_created__function_call_widen => partials(d, lbound(partials, 2):ubound(partials, 2))
    __libasr_created__result_metadata_1 = size(__libasr_created__function_call_widen, 1)
    __libasr_reduction_acc = 0.00000000e+00
    ! CheckBounds LHS = __libasr_created__function_call_widen, RHS = __libasr_created__function_call_widen, factors, 
    __libasr_index_0_1 = lbound(factors, 1)
    do __libasr_index_0_ = lbound(__libasr_created__function_call_widen, 1),&
         ubound(__libasr_created__function_call_widen, 1)
        __libasr_reduction_acc = __libasr_reduction_acc + widen(__libasr_created__function_call_widen(__libasr_index_0_)&
        )*factors(__libasr_index_0_1)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
    __libasr_created__intrinsic_array_function_Sum = __libasr_reduction_acc
    out_col = halve(__libasr_created__intrinsic_array_function_Sum/total)
end subroutine array_op_reduction_nested_call

