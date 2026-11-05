module array_op_reduction_elemental_call_mod
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

end module array_op_reduction_elemental_call_mod

subroutine array_op_reduction_elemental_call(k, q, s, m, c, d, h)
    use array_op_reduction_elemental_call_mod, only: halve
    use array_op_reduction_elemental_call_mod, only: widen
    integer(2), dimension(:), pointer :: __libasr_created__function_call_widen
    integer(2), dimension(:), pointer :: __libasr_created__function_call_widen1
    integer(2), dimension(:), pointer :: __libasr_created__function_call_widen2
    integer(2), dimension(:), pointer :: __libasr_created__function_call_widen3
    real(4) :: __libasr_created__intrinsic_array_function_Sum
    integer(4) :: __libasr_created__result_metadata_
    integer(4) :: __libasr_created__result_metadata_1
    integer(4) :: __libasr_created__result_metadata_10
    integer(4) :: __libasr_created__result_metadata_11
    integer(4) :: __libasr_created__result_metadata_12
    integer(4) :: __libasr_created__result_metadata_13
    integer(4) :: __libasr_created__result_metadata_14
    integer(4) :: __libasr_created__result_metadata_15
    integer(4) :: __libasr_created__result_metadata_2
    integer(4) :: __libasr_created__result_metadata_3
    integer(4) :: __libasr_created__result_metadata_4
    integer(4) :: __libasr_created__result_metadata_5
    integer(4) :: __libasr_created__result_metadata_6
    integer(4) :: __libasr_created__result_metadata_7
    integer(4) :: __libasr_created__result_metadata_8
    integer(4) :: __libasr_created__result_metadata_9
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4) :: __libasr_index_0_3
    integer(4) :: __libasr_index_0_4
    integer(4) :: __libasr_index_0_5
    integer(4) :: __libasr_index_0_6
    integer(4) :: __libasr_index_0_7
    real(4) :: __libasr_reduction_acc
    real(4) :: __libasr_reduction_acc1
    integer(4) :: __libasr_reduction_acc2
    real(4) :: __libasr_reduction_acc3
    real(4) :: __libasr_reduction_acc4
    real(4) :: __libasr_reduction_element
    integer(4), dimension(:), intent(out) :: c
    real(4), intent(out) :: d
    real(4), dimension(:), intent(out) :: h
    integer(2), dimension(:, :), intent(in) :: k
    real(4), intent(out) :: m
    real(4), dimension(:), intent(in) :: q
    real(4), dimension(:), intent(out) :: s
    integer(4) :: t
    do t = 1, size(k, 2)
        __libasr_created__result_metadata_ = (0 > (ubound(k, 1) - lbound(k, 1) + 1)/1 ? 0 : (ubound(k, 1) - lbound(k,&
         1) + 1)/1)
        __libasr_created__function_call_widen => k(lbound(k, 1):ubound(k, 1), t)
        __libasr_created__result_metadata_6 = size(__libasr_created__function_call_widen, 1)
        __libasr_created__result_metadata_7 = size(__libasr_created__function_call_widen, 1)
        __libasr_reduction_acc = 0.00000000e+00
        ! CheckBounds LHS = q, RHS = q, __libasr_created__function_call_widen, 
        __libasr_index_0_1 = lbound(__libasr_created__function_call_widen, 1)
        do __libasr_index_0_ = lbound(q, 1), ubound(q, 1)
            __libasr_reduction_acc = __libasr_reduction_acc + q(__libasr_index_0_)*widen(__libasr_created__function_call&
        _widen(__libasr_index_0_1))
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
        s(t) = __libasr_reduction_acc
    end do
    __libasr_created__result_metadata_1 = size(q, 1)
    __libasr_created__result_metadata_2 = (0 > (ubound(k, 1) - lbound(k, 1) + 1)/1 ? 0 : (ubound(k, 1) - lbound(k,&
         1) + 1)/1)
    __libasr_created__function_call_widen1 => k(lbound(k, 1):ubound(k, 1), 1)
    __libasr_created__result_metadata_8 = size(q, 1)
    __libasr_created__result_metadata_9 = size(__libasr_created__function_call_widen1, 1)
    __libasr_created__result_metadata_10 = size(q, 1)
    __libasr_created__result_metadata_11 = size(__libasr_created__function_call_widen1, 1)
    __libasr_reduction_acc1 = -3.40282347e+38
    ! CheckBounds LHS = q, RHS = q, __libasr_created__function_call_widen1, 
    __libasr_index_0_3 = lbound(__libasr_created__function_call_widen1, 1)
    do __libasr_index_0_2 = lbound(q, 1), ubound(q, 1)
        __libasr_reduction_element = halve(q(__libasr_index_0_2))*widen(__libasr_created__function_call_widen1(__libasr_&
        index_0_3))
        __libasr_reduction_acc1 = (__libasr_reduction_acc1 > __libasr_reduction_element ? __libasr_reduction_acc1 : __li&
        basr_reduction_element)
        __libasr_index_0_3 = __libasr_index_0_3 + 1
    end do
    m = __libasr_reduction_acc1
    do t = 1, size(k, 2)
        __libasr_created__result_metadata_3 = (0 > (ubound(k, 1) - lbound(k, 1) + 1)/1 ? 0 : (ubound(k, 1) - lbound(k,&
         1) + 1)/1)
        __libasr_created__function_call_widen2 => k(lbound(k, 1):ubound(k, 1), t)
        __libasr_created__result_metadata_12 = size(__libasr_created__function_call_widen2, 1)
        __libasr_created__result_metadata_13 = size(__libasr_created__function_call_widen2, 1)
        __libasr_reduction_acc2 = 0
        do __libasr_index_0_4 = lbound(__libasr_created__function_call_widen2, 1),&
         ubound(__libasr_created__function_call_widen2, 1)
            if (widen(__libasr_created__function_call_widen2(__libasr_index_0_4)) > 4.00000000e+00) then
                __libasr_reduction_acc2 = __libasr_reduction_acc2 + 1
            end if
        end do
        c(t) = __libasr_reduction_acc2
    end do
    __libasr_created__result_metadata_4 = size(q, 1)
    __libasr_created__result_metadata_14 = size(q, 1)
    __libasr_reduction_acc3 = 0.00000000e+00
    do __libasr_index_0_5 = lbound(q, 1), ubound(q, 1)
        __libasr_reduction_acc3 = __libasr_reduction_acc3 + halve(q(__libasr_index_0_5))
    end do
    d = __libasr_reduction_acc3
    do t = 1, size(k, 2)
        __libasr_created__result_metadata_5 = (0 > (ubound(k, 1) - lbound(k, 1) + 1)/1 ? 0 : (ubound(k, 1) - lbound(k,&
         1) + 1)/1)
        __libasr_created__function_call_widen3 => k(lbound(k, 1):ubound(k, 1), t)
        __libasr_created__result_metadata_15 = size(__libasr_created__function_call_widen3, 1)
        __libasr_reduction_acc4 = 0.00000000e+00
        ! CheckBounds LHS = q, RHS = q, __libasr_created__function_call_widen3, 
        __libasr_index_0_7 = lbound(__libasr_created__function_call_widen3, 1)
        do __libasr_index_0_6 = lbound(q, 1), ubound(q, 1)
            __libasr_reduction_acc4 = __libasr_reduction_acc4 + q(__libasr_index_0_6)*widen(__libasr_created__function_c&
        all_widen3(__libasr_index_0_7))
            __libasr_index_0_7 = __libasr_index_0_7 + 1
        end do
        __libasr_created__intrinsic_array_function_Sum = __libasr_reduction_acc4
        h(t) = halve(__libasr_created__intrinsic_array_function_Sum)
    end do
end subroutine array_op_reduction_elemental_call

