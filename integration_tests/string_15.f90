program string_15
implicit none
    character(len = 10) :: s1 = 'abcde'
    character(len = 10) :: s2 = 'xyz'
    character(len = 10) :: s3 = CHAR(255)

    ! lge is >=, lgt is >, lle is <=, llt is <
    if (lge(s1, s2) .neqv. .false.) error stop
    if (lgt(s1, s2) .neqv. .false.) error stop
    if (lle(s1, s2) .neqv. .true.) error stop
    if (llt(s1, s2) .neqv. .true.) error stop

    ! ensure that string comparisons are unsigned.
    if (lgt(s1, s3) .neqv. .false.) error stop
    if (lgt(s2, s3) .neqv. .false.) error stop

    print *, s1, s2
end program
