! Test: CHARACTER(*) assumed-size array via implicit interface
! MRE from issue #9381 - direct call from program to subroutine
program character_16
    implicit none
    character(1) :: adumma(1)

    adumma(1) = ' '
    call callee(adumma)
end program character_16

subroutine callee(ei)
    implicit none
    character(1) :: ei(*)

    if (ei(1) == ' ') then
        print *, 'PASS'
    else
        error stop 'FAIL: expected space character'
    end if
end subroutine callee
