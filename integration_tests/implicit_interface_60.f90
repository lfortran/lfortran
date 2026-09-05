! References from different scopes, with a CHARACTER variable and a literal,
! must agree on the external procedure's descriptor representation.
program implicit_interface_60
    implicit none
    character(len=1) :: trans
    integer :: n
    external :: report_len, call_with_literal

    trans = 'N'
    call report_len(trans, 3, n)
    if (n /= 3) error stop

    call call_with_literal(n)
    if (n /= 4) error stop

    print *, "OK"
end program implicit_interface_60

subroutine call_with_literal(n)
    implicit none
    integer :: n
    external :: report_len
    call report_len('T', 4, n)
end subroutine call_with_literal

subroutine report_len(trans, k, n)
    implicit none
    character*1 trans
    integer k, n
    n = k
    if (trans /= 'N' .and. trans /= 'T') n = -1
end subroutine report_len
