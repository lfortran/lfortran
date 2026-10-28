subroutine array_op_reduction_elemental(a, b, n, s, m, c, e)
    real(4), dimension(:), allocatable :: __libasr_created__array_section_
    real(4), dimension(:), allocatable :: __libasr_created__array_section_1
    real(4), dimension(:), allocatable :: __libasr_created__array_section_2
    real(4), dimension(:), allocatable :: __libasr_created__array_section_3
    real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_
    real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_1
    real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_2
    real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_3
    real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_4
    real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_5
    real(4), dimension(:), allocatable :: __libasr_created__array_size_v
    real(4), dimension(:), allocatable :: __libasr_created__array_size_v1
    real(4), dimension(:), allocatable :: __libasr_created__intrinsic_elemental_function_Abs
    real(4), dimension(:), allocatable :: __libasr_created__intrinsic_elemental_function_Abs1
    real(4), dimension(:), allocatable :: __libasr_created__intrinsic_elemental_function_Abs2
    real(4), dimension(:), allocatable :: __libasr_created__intrinsic_elemental_function_Log
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_10
    integer(4) :: __libasr_index_0_11
    integer(4) :: __libasr_index_0_12
    integer(4) :: __libasr_index_0_13
    integer(4) :: __libasr_index_0_14
    integer(4) :: __libasr_index_0_15
    integer(4) :: __libasr_index_0_16
    integer(4) :: __libasr_index_0_17
    integer(4) :: __libasr_index_0_18
    integer(4) :: __libasr_index_0_19
    integer(4) :: __libasr_index_0_2
    integer(4) :: __libasr_index_0_20
    integer(4) :: __libasr_index_0_21
    integer(4) :: __libasr_index_0_22
    integer(4) :: __libasr_index_0_23
    integer(4) :: __libasr_index_0_24
    integer(4) :: __libasr_index_0_25
    integer(4) :: __libasr_index_0_26
    integer(4) :: __libasr_index_0_27
    integer(4) :: __libasr_index_0_28
    integer(4) :: __libasr_index_0_29
    integer(4) :: __libasr_index_0_3
    integer(4) :: __libasr_index_0_30
    integer(4) :: __libasr_index_0_4
    integer(4) :: __libasr_index_0_5
    integer(4) :: __libasr_index_0_6
    integer(4) :: __libasr_index_0_7
    integer(4) :: __libasr_index_0_8
    integer(4) :: __libasr_index_0_9
    real(4) :: __libasr_reduction_acc
    real(4) :: __libasr_reduction_acc1
    integer(4) :: __libasr_reduction_acc2
    real(4) :: __libasr_reduction_acc3
    real(4), dimension(:), intent(in) :: a
    real(4), dimension(:), intent(in) :: b
    integer(4), intent(out) :: c
    real(4), intent(out) :: e
    real(4), intent(out) :: m
    integer(4), intent(in) :: n
    real(4), intent(out) :: s
    deallocate(__libasr_created__intrinsic_elemental_function_Abs)
    deallocate(__libasr_created__array_size_v)
    allocate(__libasr_created__array_size_v((n - 1)/1 + 1))
__libasr_created__array_section_pointer_ => a(1:n)
    ! CheckBounds LHS = __libasr_created__array_size_v, RHS = __libasr_created__array_section_pointer_, 
    __libasr_index_0_1 = lbound(__libasr_created__array_section_pointer_, 1)
    do __libasr_index_0_ = lbound(__libasr_created__array_size_v, 1), ubound(__libasr_created__array_size_v, 1)
        __libasr_created__array_size_v(__libasr_index_0_) = __libasr_created__array_section_pointer_(__libasr_index_0_1)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
    allocate(__libasr_created__intrinsic_elemental_function_Abs(size(__libasr_created__array_size_v, 1)))
    deallocate(__libasr_created__array_section_)
    allocate(__libasr_created__array_section_((n - 1)/1 + 1))
__libasr_created__array_section_pointer_1 => a(1:n)
    ! CheckBounds LHS = __libasr_created__array_section_, RHS = __libasr_created__array_section_pointer_1, 
    __libasr_index_0_3 = lbound(__libasr_created__array_section_pointer_1, 1)
    do __libasr_index_0_2 = lbound(__libasr_created__array_section_, 1), ubound(__libasr_created__array_section_, 1)
        __libasr_created__array_section_(__libasr_index_0_2) = __libasr_created__array_section_pointer_1(__libasr_index_&
        0_3)
        __libasr_index_0_3 = __libasr_index_0_3 + 1
    end do
    deallocate(__libasr_created__array_section_1)
    allocate(__libasr_created__array_section_1((n - 1)/1 + 1))
__libasr_created__array_section_pointer_2 => b(1:n)
    ! CheckBounds LHS = __libasr_created__array_section_1, RHS = __libasr_created__array_section_pointer_2, 
    __libasr_index_0_5 = lbound(__libasr_created__array_section_pointer_2, 1)
    do __libasr_index_0_4 = lbound(__libasr_created__array_section_1, 1), ubound(__libasr_created__array_section_1, 1)
        __libasr_created__array_section_1(__libasr_index_0_4) = __libasr_created__array_section_pointer_2(__libasr_index&
        _0_5)
        __libasr_index_0_5 = __libasr_index_0_5 + 1
    end do
    ! CheckBounds LHS = __libasr_created__intrinsic_elemental_function_Abs, RHS = __libasr_created__array_section_,&
         __libasr_created__array_section_1, 
    __libasr_index_0_7 = lbound(__libasr_created__array_section_, 1)
    __libasr_index_0_8 = lbound(__libasr_created__array_section_1, 1)
    do __libasr_index_0_6 = lbound(__libasr_created__intrinsic_elemental_function_Abs, 1),&
         ubound(__libasr_created__intrinsic_elemental_function_Abs, 1)
        __libasr_created__intrinsic_elemental_function_Abs(__libasr_index_0_6) = __libasr_created__array_section_(__liba&
        sr_index_0_7) - __libasr_created__array_section_1(__libasr_index_0_8)
        __libasr_index_0_7 = __libasr_index_0_7 + 1
        __libasr_index_0_8 = __libasr_index_0_8 + 1
    end do
    __libasr_reduction_acc = 0.00000000e+00
    do __libasr_index_0_9 = lbound(__libasr_created__intrinsic_elemental_function_Abs, 1),&
         ubound(__libasr_created__intrinsic_elemental_function_Abs, 1)
        __libasr_reduction_acc = __libasr_reduction_acc + Abs(__libasr_created__intrinsic_elemental_function_Abs(__libas&
        r_index_0_9))
    end do
    s = __libasr_reduction_acc
    deallocate(__libasr_created__intrinsic_elemental_function_Abs1)
    deallocate(__libasr_created__array_size_v1)
    allocate(__libasr_created__array_size_v1((n - 2)/1 + 1))
__libasr_created__array_section_pointer_3 => a(2:n)
    ! CheckBounds LHS = __libasr_created__array_size_v1, RHS = __libasr_created__array_section_pointer_3, 
    __libasr_index_0_11 = lbound(__libasr_created__array_section_pointer_3, 1)
    do __libasr_index_0_10 = lbound(__libasr_created__array_size_v1, 1), ubound(__libasr_created__array_size_v1, 1)
        __libasr_created__array_size_v1(__libasr_index_0_10) = __libasr_created__array_section_pointer_3(__libasr_index_&
        0_11)
        __libasr_index_0_11 = __libasr_index_0_11 + 1
    end do
    allocate(__libasr_created__intrinsic_elemental_function_Abs1(size(__libasr_created__array_size_v1, 1)))
    deallocate(__libasr_created__array_section_2)
    allocate(__libasr_created__array_section_2((n - 2)/1 + 1))
__libasr_created__array_section_pointer_4 => a(2:n)
    ! CheckBounds LHS = __libasr_created__array_section_2, RHS = __libasr_created__array_section_pointer_4, 
    __libasr_index_0_13 = lbound(__libasr_created__array_section_pointer_4, 1)
    do __libasr_index_0_12 = lbound(__libasr_created__array_section_2, 1), ubound(__libasr_created__array_section_2, 1)
        __libasr_created__array_section_2(__libasr_index_0_12) = __libasr_created__array_section_pointer_4(__libasr_inde&
        x_0_13)
        __libasr_index_0_13 = __libasr_index_0_13 + 1
    end do
    deallocate(__libasr_created__array_section_3)
    allocate(__libasr_created__array_section_3((n - 1 - 1)/1 + 1))
__libasr_created__array_section_pointer_5 => a(1:n - 1)
    ! CheckBounds LHS = __libasr_created__array_section_3, RHS = __libasr_created__array_section_pointer_5, 
    __libasr_index_0_15 = lbound(__libasr_created__array_section_pointer_5, 1)
    do __libasr_index_0_14 = lbound(__libasr_created__array_section_3, 1), ubound(__libasr_created__array_section_3, 1)
        __libasr_created__array_section_3(__libasr_index_0_14) = __libasr_created__array_section_pointer_5(__libasr_inde&
        x_0_15)
        __libasr_index_0_15 = __libasr_index_0_15 + 1
    end do
    ! CheckBounds LHS = __libasr_created__intrinsic_elemental_function_Abs1, RHS = __libasr_created__array_section_2,&
         __libasr_created__array_section_3, 
    __libasr_index_0_17 = lbound(__libasr_created__array_section_2, 1)
    __libasr_index_0_18 = lbound(__libasr_created__array_section_3, 1)
    do __libasr_index_0_16 = lbound(__libasr_created__intrinsic_elemental_function_Abs1, 1),&
         ubound(__libasr_created__intrinsic_elemental_function_Abs1, 1)
        __libasr_created__intrinsic_elemental_function_Abs1(__libasr_index_0_16) = __libasr_created__array_section_2(__l&
        ibasr_index_0_17) - __libasr_created__array_section_3(__libasr_index_0_18)
        __libasr_index_0_17 = __libasr_index_0_17 + 1
        __libasr_index_0_18 = __libasr_index_0_18 + 1
    end do
    __libasr_reduction_acc1 = -3.40282347e+38
    ! CheckBounds LHS = __libasr_created__intrinsic_elemental_function_Abs1,&
         RHS = __libasr_created__intrinsic_elemental_function_Abs1, __libasr_created__intrinsic_elemental_function_Abs1,&
         
    __libasr_index_0_20 = lbound(__libasr_created__intrinsic_elemental_function_Abs1, 1)
    do __libasr_index_0_19 = lbound(__libasr_created__intrinsic_elemental_function_Abs1, 1),&
         ubound(__libasr_created__intrinsic_elemental_function_Abs1, 1)
        __libasr_reduction_acc1 = (__libasr_reduction_acc1 > Abs(__libasr_created__intrinsic_elemental_function_Abs1(__l&
        ibasr_index_0_19)) ? __libasr_reduction_acc1 : Abs(__libasr_created__intrinsic_elemental_function_Abs1(__libasr_index_0&
        _20)))
        __libasr_index_0_20 = __libasr_index_0_20 + 1
    end do
    m = __libasr_reduction_acc1
    deallocate(__libasr_created__intrinsic_elemental_function_Abs2)
    allocate(__libasr_created__intrinsic_elemental_function_Abs2(size(a, 1)))
    ! CheckBounds LHS = __libasr_created__intrinsic_elemental_function_Abs2, RHS = a, b, 
    __libasr_index_0_22 = lbound(a, 1)
    __libasr_index_0_23 = lbound(b, 1)
    do __libasr_index_0_21 = lbound(__libasr_created__intrinsic_elemental_function_Abs2, 1),&
         ubound(__libasr_created__intrinsic_elemental_function_Abs2, 1)
        __libasr_created__intrinsic_elemental_function_Abs2(__libasr_index_0_21) = a(__libasr_index_0_22) - b(__libasr_i&
        ndex_0_23)
        __libasr_index_0_22 = __libasr_index_0_22 + 1
        __libasr_index_0_23 = __libasr_index_0_23 + 1
    end do
    __libasr_reduction_acc2 = 0
    do __libasr_index_0_24 = lbound(__libasr_created__intrinsic_elemental_function_Abs2, 1),&
         ubound(__libasr_created__intrinsic_elemental_function_Abs2, 1)
        if (Abs(__libasr_created__intrinsic_elemental_function_Abs2(__libasr_index_0_24)) > 5.00000000e-01) then
            __libasr_reduction_acc2 = __libasr_reduction_acc2 + 1
        end if
    end do
    c = __libasr_reduction_acc2
    deallocate(__libasr_created__intrinsic_elemental_function_Log)
    allocate(__libasr_created__intrinsic_elemental_function_Log(size(b, 1)))
    ! CheckBounds LHS = __libasr_created__intrinsic_elemental_function_Log, RHS = b, 
    __libasr_index_0_26 = lbound(b, 1)
    do __libasr_index_0_25 = lbound(__libasr_created__intrinsic_elemental_function_Log, 1),&
         ubound(__libasr_created__intrinsic_elemental_function_Log, 1)
        __libasr_created__intrinsic_elemental_function_Log(__libasr_index_0_25) = b(__libasr_index_0_26) + real(1,&
         kind=4)
        __libasr_index_0_26 = __libasr_index_0_26 + 1
    end do
    __libasr_reduction_acc3 = 3.40282347e+38
    ! CheckBounds LHS = a, RHS = a, __libasr_created__intrinsic_elemental_function_Log, a,&
         __libasr_created__intrinsic_elemental_function_Log, 
    __libasr_index_0_28 = lbound(__libasr_created__intrinsic_elemental_function_Log, 1)
    __libasr_index_0_29 = lbound(a, 1)
    __libasr_index_0_30 = lbound(__libasr_created__intrinsic_elemental_function_Log, 1)
    do __libasr_index_0_27 = lbound(a, 1), ubound(a, 1)
        __libasr_reduction_acc3 = (__libasr_reduction_acc3 < Exp(a(__libasr_index_0_27))*Log(__libasr_created__intrinsic&
        _elemental_function_Log(__libasr_index_0_28)) ? __libasr_reduction_acc3 : Exp(a(__libasr_index_0_29))*Log(__libasr_crea&
        ted__intrinsic_elemental_function_Log(__libasr_index_0_30)))
        __libasr_index_0_28 = __libasr_index_0_28 + 1
        __libasr_index_0_29 = __libasr_index_0_29 + 1
        __libasr_index_0_30 = __libasr_index_0_30 + 1
    end do
    e = __libasr_reduction_acc3
end subroutine array_op_reduction_elemental

subroutine array_op_reduction_loop(a, b, i, j, s, n)
    logical(4) :: __libasr_created__intrinsic_array_function_Any
    real(4) :: __libasr_created__intrinsic_array_function_Sum
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4) :: __libasr_index_0_3
    integer(4) :: __libasr_index_0_4
    integer(4) :: __libasr_index_0_5
    real(4) :: __libasr_reduction_acc
    logical(4) :: __libasr_reduction_acc1
    integer(4) :: __libasr_reduction_acc2
    real(4) :: __libasr_section_first_element_
    real(4) :: __libasr_section_first_element_1
    real(4) :: __libasr_section_first_element_2
    real(4) :: __libasr_section_first_element_3
    real(4) :: __libasr_section_first_element_4
    real(4) :: __libasr_section_first_element_5
    real(4) :: __libasr_section_last_element_
    real(4) :: __libasr_section_last_element_1
    real(4) :: __libasr_section_last_element_2
    real(4) :: __libasr_section_last_element_3
    real(4) :: __libasr_section_last_element_4
    real(4) :: __libasr_section_last_element_5
    real(4), dimension(:), intent(in) :: a
    real(4), dimension(:), intent(in) :: b
    integer(4), intent(in) :: i
    integer(4), intent(in) :: j
    integer(4), intent(out) :: n
    real(4), intent(out) :: s
    __libasr_reduction_acc = 0.00000000e+00
    ! CheckBounds LHS = a(i:j), RHS = a(i:j), b(i:j), 
    if (i <= j) then
        __libasr_section_first_element_ = a(i)
        __libasr_section_last_element_ = a(j)
    end if
    if (i <= j) then
        __libasr_section_first_element_1 = b(i)
        __libasr_section_last_element_1 = b(j)
    end if
    __libasr_index_0_1 = i
    do __libasr_index_0_ = i, j
        __libasr_reduction_acc = __libasr_reduction_acc + a(__libasr_index_0_)*b(__libasr_index_0_1)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
    __libasr_created__intrinsic_array_function_Sum = __libasr_reduction_acc
    s = __libasr_created__intrinsic_array_function_Sum + real(1, kind=4)
    n = 0
    __libasr_reduction_acc1 = .false.
    ! CheckBounds LHS = a(i:j), RHS = a(i:j), b(i:j), 
    if (i <= j) then
        __libasr_section_first_element_2 = a(i)
        __libasr_section_last_element_2 = a(j)
    end if
    if (i <= j) then
        __libasr_section_first_element_3 = b(i)
        __libasr_section_last_element_3 = b(j)
    end if
    __libasr_index_0_3 = i
    do __libasr_index_0_2 = i, j
        __libasr_reduction_acc1 = __libasr_reduction_acc1 .or. a(__libasr_index_0_2) > b(__libasr_index_0_3)
        __libasr_index_0_3 = __libasr_index_0_3 + 1
    end do
    __libasr_created__intrinsic_array_function_Any = __libasr_reduction_acc1
    if (__libasr_created__intrinsic_array_function_Any) then
        __libasr_reduction_acc2 = 0
        ! CheckBounds LHS = a(i:j), RHS = a(i:j), b(i:j), 
        if (i <= j) then
            __libasr_section_first_element_4 = a(i)
            __libasr_section_last_element_4 = a(j)
        end if
        if (i <= j) then
            __libasr_section_first_element_5 = b(i)
            __libasr_section_last_element_5 = b(j)
        end if
        __libasr_index_0_5 = i
        do __libasr_index_0_4 = i, j
            if (a(__libasr_index_0_4) > real(2, kind=4)*b(__libasr_index_0_5)) then
                __libasr_reduction_acc2 = __libasr_reduction_acc2 + 1
            end if
            __libasr_index_0_5 = __libasr_index_0_5 + 1
        end do
        n = __libasr_reduction_acc2
    end if
end subroutine array_op_reduction_loop

