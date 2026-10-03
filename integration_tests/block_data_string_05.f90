! Test: assigning to a CHARACTER variable in a COMMON block that was
! initialized via DATA in a separate BLOCK DATA unit.
! The DATA-initialized common block must live in writable memory, so a
! later assignment to the CHARACTER member does not fault at runtime
! (regression test: previously crashed with a Bus error because the
! backing string data was placed in read-only memory).

block data bd
    implicit none
    character(4) :: s
    common /cb/ s
    data s /'ab'/
end block data

program block_data_string_05
    implicit none
    character(4) :: s
    common /cb/ s

    ! Value from the DATA statement (blank-padded to length 4).
    if (s /= 'ab  ') error stop

    ! Overwriting the common CHARACTER member must succeed.
    s = 'xy'
    if (s /= 'xy  ') error stop

    print *, s
end program
