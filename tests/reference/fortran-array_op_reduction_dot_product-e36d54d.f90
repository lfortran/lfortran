subroutine dot_products(a, b, ia, ca, cb, la, lb, i, j, s, is, cs, ls)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4) :: __libasr_index_0_3
    integer(4) :: __libasr_index_0_4
    integer(4) :: __libasr_index_0_5
    integer(4) :: __libasr_index_0_6
    integer(4) :: __libasr_index_0_7
    integer(4) :: __libasr_index_0_8
    real(4) :: __libasr_reduction_acc
    integer(4) :: __libasr_reduction_acc1
    complex(4) :: __libasr_reduction_acc2
    logical(4) :: __libasr_reduction_acc3
    real(4) :: __libasr_section_first_element_
    real(4) :: __libasr_section_first_element_1
    integer(4) :: __libasr_section_first_element_2
    integer(4) :: __libasr_section_first_element_3
    real(4) :: __libasr_section_last_element_
    real(4) :: __libasr_section_last_element_1
    integer(4) :: __libasr_section_last_element_2
    integer(4) :: __libasr_section_last_element_3
    real(4), dimension(:), intent(in) :: a
    real(4), dimension(10), intent(in) :: b
    complex(4), dimension(:), intent(in) :: ca
    complex(4), dimension(:), intent(in) :: cb
    complex(4), intent(out) :: cs
    integer(4), intent(in) :: i
    integer(4), dimension(:), intent(in) :: ia
    integer(4), intent(out) :: is
    integer(4), intent(in) :: j
    logical(4), dimension(:), intent(in) :: la
    logical(4), dimension(:), intent(in) :: lb
    logical(4), intent(out) :: ls
    real(4), intent(out) :: s
    __libasr_reduction_acc = 0.00000000e+00
    ! CheckBounds LHS = a(i:j), RHS = a(i:j), b(j:i:-1), 
    if (i <= j) then
        __libasr_section_first_element_ = a(i)
        __libasr_section_last_element_ = a(j)
    end if
    if (j >= i) then
        __libasr_section_first_element_1 = b(j)
        __libasr_section_last_element_1 = b(j + (i - j)/((-1))*(-1))
    end if
    __libasr_index_0_1 = j
    do __libasr_index_0_ = i, j
        __libasr_reduction_acc = __libasr_reduction_acc + a(__libasr_index_0_)*real(2, kind=4)*b(__libasr_index_0_1)
        __libasr_index_0_1 = __libasr_index_0_1 + (-1)
    end do
    s = __libasr_reduction_acc
    __libasr_reduction_acc1 = 0
    __libasr_section_first_element_2 = ia(1)
    __libasr_section_last_element_2 = ia(1 + (7 - 1)/3*3)
    __libasr_section_first_element_3 = ia(7)
    __libasr_section_last_element_3 = ia(7 + (1 - 7)/((-3))*(-3))
    __libasr_index_0_3 = 7
    do __libasr_index_0_2 = 1, 7, 3
        __libasr_reduction_acc1 = __libasr_reduction_acc1 + (ia(__libasr_index_0_2) + 1)*ia(__libasr_index_0_3)
        __libasr_index_0_3 = __libasr_index_0_3 + (-3)
    end do
    is = __libasr_reduction_acc1
    __libasr_reduction_acc2 = (0.00000000e+00, 0.00000000e+00)
    ! CheckBounds LHS = ca, RHS = ca, cb, 
    __libasr_index_0_5 = lbound(cb, 1)
    do __libasr_index_0_4 = lbound(ca, 1), ubound(ca, 1)
        __libasr_reduction_acc2 = __libasr_reduction_acc2 + Conjg(ca(__libasr_index_0_4)*cmplx(2, 0.0,&
         kind=4))*cb(__libasr_index_0_5)
        __libasr_index_0_5 = __libasr_index_0_5 + 1
    end do
    cs = __libasr_reduction_acc2
    __libasr_reduction_acc3 = .false.
    ! CheckBounds LHS = la, RHS = la, lb, lb, 
    __libasr_index_0_7 = lbound(lb, 1)
    __libasr_index_0_8 = lbound(lb, 1)
    do __libasr_index_0_6 = lbound(la, 1), ubound(la, 1)
        __libasr_reduction_acc3 = __libasr_reduction_acc3 .or. la(__libasr_index_0_6) .and. lb(__libasr_index_0_7) .and.&
         lb(__libasr_index_0_8)
        __libasr_index_0_7 = __libasr_index_0_7 + 1
        __libasr_index_0_8 = __libasr_index_0_8 + 1
    end do
    ls = __libasr_reduction_acc3
end subroutine dot_products

