program string_15
    implicit none
    character(len = 10) :: s1 = 'abcde'
    character(len = 10) :: s2 = 'xyz'

    ! lge is >=, lgt is >, lle is <=, llt is <
    if (lge(s1, s2) .neqv. .false.) error stop
    if (lgt(s1, s2) .neqv. .false.) error stop
    if (lle(s1, s2) .neqv. .true.) error stop
    if (llt(s1, s2) .neqv. .true.) error stop

    print *, s1, s2
end program
