module array_op_same_element_section_mod
implicit none

contains

elemental real(4) function halve(x) result(y)
    real(4), intent(in) :: x
    y = 5.00000000e-01*x
end function halve

end module array_op_same_element_section_mod

subroutine array_op_same_element_section(scores, visible, total)
    use array_op_same_element_section_mod, only: halve
    real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_
    integer(4) :: __libasr_created__array_section_subscript_
    integer(4) :: __libasr_created__array_section_subscript_1
    integer(4) :: __libasr_created__array_section_subscript_2
    real(4), dimension(:), pointer :: __libasr_created__intrinsic_array_function_MaxVal
    real(4), dimension(:), allocatable :: __libasr_created__intrinsic_elemental_function_Exp
    integer(4) :: __libasr_created__result_metadata_
    integer(4) :: __libasr_created__result_metadata_1
    real(4), dimension(:), pointer :: __libasr_created_real_binop_left_
    real(4), dimension(:), pointer :: __libasr_created_real_binop_left_1
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4) :: __libasr_index_0_3
    integer(4) :: __libasr_index_0_4
    integer(4) :: __libasr_index_0_5
    integer(4) :: __libasr_index_0_6
    integer(4) :: __libasr_index_0_7
    real(4) :: m
    real(4), dimension(:), intent(inout) :: scores
    real(4), intent(in) :: total
    integer(4), intent(in) :: visible
    __libasr_created__intrinsic_array_function_MaxVal => scores(lbound(scores, 1):visible)
    m = MaxVal(__libasr_created__intrinsic_array_function_MaxVal)
    __libasr_created_real_binop_left_ => scores(lbound(scores, 1):visible)
    __libasr_created__array_section_subscript_ = visible
    ! CheckBounds LHS = scores(lbound(scores, 1):__libasr_created__array_section_subscript_),&
         RHS = __libasr_created_real_binop_left_, 
    __libasr_index_0_1 = lbound(__libasr_created_real_binop_left_, 1)
    do __libasr_index_0_ = lbound(scores, 1), __libasr_created__array_section_subscript_
        scores(__libasr_index_0_) = Exp(__libasr_created_real_binop_left_(__libasr_index_0_1) - m)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
    __libasr_created_real_binop_left_1 => scores(lbound(scores, 1):visible)
    __libasr_created__result_metadata_ = size(__libasr_created_real_binop_left_1, 1)
    __libasr_created__array_section_subscript_1 = visible
    __libasr_created__result_metadata_1 = size(__libasr_created_real_binop_left_1, 1)
    ! CheckBounds LHS = scores(lbound(scores, 1):__libasr_created__array_section_subscript_1),&
         RHS = __libasr_created_real_binop_left_1, 
    __libasr_index_0_3 = lbound(__libasr_created_real_binop_left_1, 1)
    do __libasr_index_0_2 = lbound(scores, 1), __libasr_created__array_section_subscript_1
        scores(__libasr_index_0_2) = halve(__libasr_created_real_binop_left_1(__libasr_index_0_3)/total)
        __libasr_index_0_3 = __libasr_index_0_3 + 1
    end do
    deallocate(__libasr_created__intrinsic_elemental_function_Exp)
    allocate(__libasr_created__intrinsic_elemental_function_Exp((visible - 1 - 1)/1 + 1))
    __libasr_created__array_section_pointer_ => scores(1:visible - 1)
    ! CheckBounds LHS = __libasr_created__intrinsic_elemental_function_Exp,&
         RHS = __libasr_created__array_section_pointer_, 
    __libasr_index_0_5 = lbound(__libasr_created__array_section_pointer_, 1)
    do __libasr_index_0_4 = lbound(__libasr_created__intrinsic_elemental_function_Exp, 1),&
         ubound(__libasr_created__intrinsic_elemental_function_Exp, 1)
        __libasr_created__intrinsic_elemental_function_Exp(__libasr_index_0_4) = __libasr_created__array_section_pointer&
        _(__libasr_index_0_5)
        __libasr_index_0_5 = __libasr_index_0_5 + 1
    end do
    __libasr_created__array_section_subscript_2 = visible
    ! CheckBounds LHS = scores(2:__libasr_created__array_section_subscript_2),&
         RHS = __libasr_created__intrinsic_elemental_function_Exp, 
    __libasr_index_0_7 = lbound(__libasr_created__intrinsic_elemental_function_Exp, 1)
    do __libasr_index_0_6 = 2, __libasr_created__array_section_subscript_2
        scores(__libasr_index_0_6) = Exp(__libasr_created__intrinsic_elemental_function_Exp(__libasr_index_0_7))
        __libasr_index_0_7 = __libasr_index_0_7 + 1
    end do
end subroutine array_op_same_element_section

