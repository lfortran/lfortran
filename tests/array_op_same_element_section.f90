module array_op_same_element_section_mod
implicit none
contains

    pure elemental function halve(x) result(y)
        real(4), intent(in) :: x
        real(4) :: y

        y = 0.5_4*x
    end function halve

end module array_op_same_element_section_mod

! An assignment whose value reads the same elements of the target through an
! intrinsic elemental function or a pure elemental call is carried out element
! by element. A value that reads other elements of the target, as a shifted
! section does, is not.
subroutine array_op_same_element_section(scores, visible, total)
    use array_op_same_element_section_mod, only: halve
    implicit none
    real(4), intent(inout) :: scores(:)
    real(4), intent(in) :: total
    integer, intent(in) :: visible
    real(4) :: m

    m = maxval(scores(:visible))
    scores(:visible) = exp(scores(:visible)-m)
    scores(:visible) = halve(scores(:visible)/total)
    scores(2:visible) = exp(scores(1:visible-1))
end subroutine array_op_same_element_section
