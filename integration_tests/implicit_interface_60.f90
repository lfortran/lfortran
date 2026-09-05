! The same external procedure with a CHARACTER dummy is referenced from two
! scopes, once with a CHARACTER variable and once with a CHARACTER literal.
! The two references synthesize different signatures, so LFortran replaces the
! interface symbol and re-duplicates it into each referencing scope through
! ASRUtils::SymbolDuplicator.
!
! That duplication has to carry FunctionType's external-ABI flag over. When it
! did not, the call sites were emitted with the string-descriptor ABI while the
! definition used the classic external ABI, the two disagreed on the argument
! count, and codegen aborted in declare_args (a Debug assertion, a segfault in
! Release).
!
! Reduced from Reference-LAPACK's BLAS/TESTING/sblat2.f, where SMVCH -- whose
! first dummy is CHARACTER*1 -- is called from the main program with the
! variable TRANS and from SCHK6 with the literal 'N'.
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
