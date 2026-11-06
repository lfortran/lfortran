module in_place_sections_ir
implicit none
type :: layer
    real(4), dimension(:, :, :), allocatable :: gradient
    real(4), dimension(:, :, :), allocatable :: kernel
end type layer

contains

subroutine update(self, work, input, n, k)
    integer(4) :: __libasr_created__array_section_subscript_
    integer(4) :: __libasr_created__array_section_subscript_1
    integer(4) :: __libasr_created__array_section_subscript_2
    integer(4) :: __libasr_created__array_section_subscript_3
    integer(4) :: __libasr_created__array_section_subscript_4
    integer(4) :: __libasr_created__array_section_subscript_5
    integer(4) :: __libasr_created__array_section_subscript_6
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4) :: __libasr_index_0_3
    integer(4) :: __libasr_index_0_4
    integer(4) :: __libasr_index_0_5
    integer(4) :: __libasr_index_0_6
    integer(4) :: __libasr_index_1_
    integer(4) :: __libasr_index_1_1
    integer(4) :: __libasr_index_1_2
    integer(4) :: __libasr_index_1_3
    integer(4) :: __libasr_index_1_4
    integer(4) :: __libasr_index_1_5
    integer(4) :: __libasr_index_1_6
    real(4) :: __libasr_section_first_element_
    real(4), dimension(:, :), intent(in) :: input
    integer(4), intent(in) :: k
    integer(4), intent(in) :: n
    type(layer), intent(inout) :: self
    real(4), dimension(:, :, :, :), intent(inout) :: work
    __libasr_created__array_section_subscript_ = n
    __libasr_created__array_section_subscript_1 = k
    __libasr_created__array_section_subscript_2 = n
    __libasr_created__array_section_subscript_3 = k
    ! CheckBounds LHS = work(__libasr_created__array_section_subscript_2, __libasr_created__array_section_subscript_3,&
         lbound(work, 3):ubound(work, 3), lbound(work, 4):ubound(work, 4)), RHS = input,&
         work(__libasr_created__array_section_subscript_, __libasr_created__array_section_subscript_1, lbound(work,&
         3):ubound(work, 3), lbound(work, 4):ubound(work, 4)), 
    if (lbound(work, 3) <= ubound(work, 3) .and. lbound(work, 4) <= ubound(work, 4)) then
        __libasr_section_first_element_ = work(__libasr_created__array_section_subscript_2,&
         __libasr_created__array_section_subscript_3, lbound(work, 3), lbound(work, 4))
    end if
    __libasr_index_1_1 = lbound(work, 4)
    __libasr_index_1_2 = lbound(input, 2)
    do __libasr_index_1_ = lbound(work, 4), ubound(work, 4)
        __libasr_index_0_1 = lbound(work, 3)
        __libasr_index_0_2 = lbound(input, 1)
        do __libasr_index_0_ = lbound(work, 3), ubound(work, 3)
            work(__libasr_created__array_section_subscript_2, __libasr_created__array_section_subscript_3,&
         __libasr_index_0_, __libasr_index_1_) = work(__libasr_created__array_section_subscript_,&
         __libasr_created__array_section_subscript_1, __libasr_index_0_1, __libasr_index_1_1) + input(__libasr_index_0_2,&
         __libasr_index_1_2)*5.00000000e-01
            __libasr_index_0_1 = __libasr_index_0_1 + 1
            __libasr_index_0_2 = __libasr_index_0_2 + 1
        end do
        __libasr_index_1_1 = __libasr_index_1_1 + 1
        __libasr_index_1_2 = __libasr_index_1_2 + 1
    end do
    __libasr_created__array_section_subscript_4 = k
    __libasr_created__array_section_subscript_5 = k
    __libasr_created__array_section_subscript_6 = k
    ! CheckBounds LHS = self%gradient(__libasr_created__array_section_subscript_6, 1:3, 1:3), RHS = input,&
         self%gradient(__libasr_created__array_section_subscript_4, 1:3, 1:3),&
         self%kernel(__libasr_created__array_section_subscript_5, 1:3, 1:3), 
    __libasr_index_1_4 = 1
    __libasr_index_1_5 = lbound(input, 2)
    __libasr_index_1_6 = 1
    do __libasr_index_1_3 = 1, 3
        __libasr_index_0_4 = 1
        __libasr_index_0_5 = lbound(input, 1)
        __libasr_index_0_6 = 1
        do __libasr_index_0_3 = 1, 3
            self%gradient(__libasr_created__array_section_subscript_6, __libasr_index_0_3,&
         __libasr_index_1_3) = self%gradient(__libasr_created__array_section_subscript_4, __libasr_index_0_4,&
         __libasr_index_1_4) + input(__libasr_index_0_5,&
         __libasr_index_1_5)*self%kernel(__libasr_created__array_section_subscript_5, __libasr_index_0_6, __libasr_index_1_6)
            __libasr_index_0_4 = __libasr_index_0_4 + 1
            __libasr_index_0_5 = __libasr_index_0_5 + 1
            __libasr_index_0_6 = __libasr_index_0_6 + 1
        end do
        __libasr_index_1_4 = __libasr_index_1_4 + 1
        __libasr_index_1_5 = __libasr_index_1_5 + 1
        __libasr_index_1_6 = __libasr_index_1_6 + 1
    end do
end subroutine update

end module in_place_sections_ir

