! Test: CHARACTER(*) assumed-size array via nested EXTERNAL declaration
! This is the actual LAPACK pattern where an intermediate subroutine
! has an EXTERNAL declaration and calls through implicit interface.
! Currently FAILING - see issue #9381
!
! Pattern: main -> schkhs (has EXTERNAL slatme) -> slatme (has CHARACTER(*))
program character_18
    implicit none
    character(1) :: adumma(1)

    adumma(1) = ' '
    call schkhs(adumma)
end program character_18

subroutine schkhs(adumma)
    implicit none
    character(1) :: adumma(1)
    external :: slatme

    call slatme(adumma)
end subroutine schkhs

subroutine slatme(ei)
    implicit none
    character(1) :: ei(*)

    if (ei(1) == ' ') then
        print *, 'PASS'
    else
        error stop 'FAIL: expected space character'
    end if
end subroutine slatme
