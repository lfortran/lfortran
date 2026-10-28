module array_op_reduction_sections_mod
implicit none
type :: conv
    real(4), dimension(:, :, :, :), allocatable :: kernel
end type conv

contains

subroutine forward(self, input, z)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_1_
    integer(4) :: __libasr_index_1_1
    real(4) :: __libasr_reduction_acc
    real(4) :: __libasr_section_first_element_
    real(4) :: __libasr_section_first_element_1
    real(4) :: __libasr_section_last_element_
    real(4) :: __libasr_section_last_element_1
    real(4), dimension(:, :), intent(in) :: input
    integer(4) :: j
    integer(4) :: n
    type(conv), intent(in) :: self
    real(4), dimension(:, :), intent(out) :: z
    do j = 1, size(z, 2)
        do n = 1, size(z, 1)
            __libasr_reduction_acc = 0.00000000e+00
            ! CheckBounds LHS = self%kernel(n, j, lbound(self%kernel, 3):ubound(self%kernel, 3), lbound(self%kernel,&
         4):ubound(self%kernel, 4)), RHS = self%kernel(n, j, lbound(self%kernel, 3):ubound(self%kernel, 3),&
         lbound(self%kernel,&
         4):ubound(self%kernel, 4)), input(lbound(input, 1):ubound(input, 1), j:j + 2), 
            if (lbound(self%kernel, 3) <= ubound(self%kernel, 3) .and. lbound(self%kernel, 4) <= ubound(self%kernel,&
         4)) then
                __libasr_section_first_element_ = self%kernel(n, j, lbound(self%kernel, 3), lbound(self%kernel, 4))
                __libasr_section_last_element_ = self%kernel(n, j, ubound(self%kernel, 3), ubound(self%kernel, 4))
            end if
            if (lbound(input, 1) <= ubound(input, 1) .and. j <= j + 2) then
                __libasr_section_first_element_1 = input(lbound(input, 1), j)
                __libasr_section_last_element_1 = input(ubound(input, 1), j + 2)
            end if
            __libasr_index_1_1 = j
            do __libasr_index_1_ = lbound(self%kernel, 4), ubound(self%kernel, 4)
                __libasr_index_0_1 = lbound(input, 1)
                do __libasr_index_0_ = lbound(self%kernel, 3), ubound(self%kernel, 3)
                    __libasr_reduction_acc = __libasr_reduction_acc + self%kernel(n, j, __libasr_index_0_,&
         __libasr_index_1_)*input(__libasr_index_0_1, __libasr_index_1_1)
                    __libasr_index_0_1 = __libasr_index_0_1 + 1
                end do
                __libasr_index_1_1 = __libasr_index_1_1 + 1
            end do
            z(n, j) = __libasr_reduction_acc
        end do
    end do
end subroutine forward

subroutine strided(ia, ib, la, m, s)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4) :: __libasr_index_0_3
    integer(4) :: __libasr_index_0_4
    integer(4) :: __libasr_index_0_5
    integer(4) :: __libasr_reduction_acc
    real(4) :: __libasr_reduction_acc1
    integer(4) :: __libasr_section_first_element_
    integer(4) :: __libasr_section_first_element_1
    integer(4) :: __libasr_section_first_element_2
    integer(4) :: __libasr_section_first_element_3
    integer(4) :: __libasr_section_first_element_4
    logical(4) :: __libasr_section_first_element_5
    integer(4) :: __libasr_section_last_element_
    integer(4) :: __libasr_section_last_element_1
    integer(4) :: __libasr_section_last_element_2
    integer(4) :: __libasr_section_last_element_3
    integer(4) :: __libasr_section_last_element_4
    logical(4) :: __libasr_section_last_element_5
    integer(4), dimension(:), intent(in) :: ia
    integer(4), dimension(-2:), intent(in) :: ib
    logical(4), dimension(:), intent(in) :: la
    integer(4), intent(out) :: m
    real(4), intent(out) :: s
    __libasr_reduction_acc = -2147483648
    __libasr_section_first_element_ = ia(1)
    __libasr_section_last_element_ = ia(1 + (7 - 1)/3*3)
    __libasr_section_first_element_1 = ib(4)
    __libasr_section_last_element_1 = ib(4 + ((-2) - 4)/((-3))*(-3))
    __libasr_section_first_element_2 = ia(1)
    __libasr_section_last_element_2 = ia(1 + (7 - 1)/3*3)
    __libasr_section_first_element_3 = ib(4)
    __libasr_section_last_element_3 = ib(4 + ((-2) - 4)/((-3))*(-3))
    __libasr_index_0_1 = 4
    __libasr_index_0_2 = 1
    __libasr_index_0_3 = 4
    do __libasr_index_0_ = 1, 7, 3
        __libasr_reduction_acc = (__libasr_reduction_acc > ia(__libasr_index_0_)*ib(__libasr_index_0_1) ? __libasr_reduc&
        tion_acc : ia(__libasr_index_0_2)*ib(__libasr_index_0_3))
        __libasr_index_0_1 = __libasr_index_0_1 + (-3)
        __libasr_index_0_2 = __libasr_index_0_2 + 3
        __libasr_index_0_3 = __libasr_index_0_3 + (-3)
    end do
    m = __libasr_reduction_acc
    __libasr_reduction_acc1 = 0.00000000e+00
    __libasr_section_first_element_4 = ia(2)
    __libasr_section_last_element_4 = ia(4)
    __libasr_section_first_element_5 = la(1)
    __libasr_section_last_element_5 = la(3)
    __libasr_index_0_5 = 1
    do __libasr_index_0_4 = 2, 4
        if (la(__libasr_index_0_5)) then
            __libasr_reduction_acc1 = __libasr_reduction_acc1 + Real(ia(__libasr_index_0_4))
        end if
        __libasr_index_0_5 = __libasr_index_0_5 + 1
    end do
    s = __libasr_reduction_acc1
end subroutine strided

end module array_op_reduction_sections_mod

