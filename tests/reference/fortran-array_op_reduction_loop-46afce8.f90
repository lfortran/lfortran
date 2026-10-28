subroutine array_op_reduction_elemental(a, b, n, s, m, c, e)
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
    real(4) :: __libasr_reduction_element
    real(4) :: __libasr_reduction_element1
    real(4) :: __libasr_section_first_element_
    real(4) :: __libasr_section_first_element_1
    real(4) :: __libasr_section_first_element_2
    real(4) :: __libasr_section_first_element_3
    real(4) :: __libasr_section_last_element_
    real(4) :: __libasr_section_last_element_1
    real(4) :: __libasr_section_last_element_2
    real(4) :: __libasr_section_last_element_3
    real(4), dimension(:), intent(in) :: a
    real(4), dimension(:), intent(in) :: b
    integer(4), intent(out) :: c
    real(4), intent(out) :: e
    real(4), intent(out) :: m
    integer(4), intent(in) :: n
    real(4), intent(out) :: s
    __libasr_reduction_acc = 0.00000000e+00
    ! CheckBounds LHS = a(1:n), RHS = a(1:n), b(1:n), 
    if (1 <= n) then
        __libasr_section_first_element_ = a(1)
        __libasr_section_last_element_ = a(n)
    end if
    if (1 <= n) then
        __libasr_section_first_element_1 = b(1)
        __libasr_section_last_element_1 = b(n)
    end if
    __libasr_index_0_1 = 1
    do __libasr_index_0_ = 1, n
        __libasr_reduction_acc = __libasr_reduction_acc + Abs(a(__libasr_index_0_) - b(__libasr_index_0_1))
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
    s = __libasr_reduction_acc
    __libasr_reduction_acc1 = -3.40282347e+38
    ! CheckBounds LHS = a(2:n), RHS = a(2:n), a(1:n - 1), 
    if (2 <= n) then
        __libasr_section_first_element_2 = a(2)
        __libasr_section_last_element_2 = a(n)
    end if
    if (1 <= n - 1) then
        __libasr_section_first_element_3 = a(1)
        __libasr_section_last_element_3 = a(n - 1)
    end if
    __libasr_index_0_3 = 1
    do __libasr_index_0_2 = 2, n
        __libasr_reduction_element = Abs(a(__libasr_index_0_2) - a(__libasr_index_0_3))
        __libasr_reduction_acc1 = (__libasr_reduction_acc1 > __libasr_reduction_element ? __libasr_reduction_acc1 : __li&
        basr_reduction_element)
        __libasr_index_0_3 = __libasr_index_0_3 + 1
    end do
    m = __libasr_reduction_acc1
    __libasr_reduction_acc2 = 0
    ! CheckBounds LHS = a, RHS = a, b, 
    __libasr_index_0_5 = lbound(b, 1)
    do __libasr_index_0_4 = lbound(a, 1), ubound(a, 1)
        if (Abs(a(__libasr_index_0_4) - b(__libasr_index_0_5)) > 5.00000000e-01) then
            __libasr_reduction_acc2 = __libasr_reduction_acc2 + 1
        end if
        __libasr_index_0_5 = __libasr_index_0_5 + 1
    end do
    c = __libasr_reduction_acc2
    __libasr_reduction_acc3 = 3.40282347e+38
    ! CheckBounds LHS = a, RHS = a, b, 
    __libasr_index_0_7 = lbound(b, 1)
    do __libasr_index_0_6 = lbound(a, 1), ubound(a, 1)
        __libasr_reduction_element1 = Exp(a(__libasr_index_0_6))*(Log(b(__libasr_index_0_7) + real(1, kind=4)))
        __libasr_reduction_acc3 = (__libasr_reduction_acc3 < __libasr_reduction_element1 ? __libasr_reduction_acc3 : __l&
        ibasr_reduction_element1)
        __libasr_index_0_7 = __libasr_index_0_7 + 1
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

