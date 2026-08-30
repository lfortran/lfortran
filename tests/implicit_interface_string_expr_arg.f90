! A character *expression* - a concatenation with a runtime-length operand, a
! substring with computed bounds - passed to a procedure with an implicit
! interface. The dummy synthesized for it must be assumed length
! (`character(len=*)`), the form the callee would have declared, not deferred
! length, which is only legal for an allocatable or a pointer.
subroutine implicit_interface_string_expr_arg(text, line, istart, istop)
    character(len=16) :: text
    character(len=200) :: line
    character(len=8) :: words(3)
    integer :: istart, istop
    call ustop('Invalid '//trim(text))
    call ustop('Invalid '//trim(adjustl(text))//' option: '//line(istart:istop))
    call ustop(line(istart:istop)//' ')
    call ustop(trim(text)//trim(line))
    call ustop_many(trim(text)//' ', words//trim(text))
end subroutine implicit_interface_string_expr_arg
