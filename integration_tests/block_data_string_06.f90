! Test: assigning to an element of a CHARACTER *array* in a COMMON block that
! was initialized via DATA in a separate BLOCK DATA unit.
! The DATA-initialized common block must live in writable memory, so a later
! assignment to an element of the CHARACTER array does not fault at runtime
! (regression test: previously crashed with a Bus error because the backing
! string data of the array was placed in read-only memory).

block data bd
    implicit none
    character(31) :: dimnam(2)
    common /cdims/ dimnam
    data dimnam /'time', 'lat'/
end block data

program block_data_string_06
    implicit none
    character(31) :: dimnam(2)
    common /cdims/ dimnam

    ! Values from the DATA statement (blank-padded to length 31).
    if (dimnam(1) /= 'time') error stop
    if (dimnam(2) /= 'lat') error stop

    ! Overwriting an element of the common CHARACTER array must succeed.
    dimnam(2) = 'latitude'
    if (dimnam(2) /= 'latitude') error stop

    ! The first element must be untouched.
    if (dimnam(1) /= 'time') error stop

    print *, dimnam(1)
    print *, dimnam(2)
end program
