program int_file
    implicit none
    character(32) :: string
    integer :: ival, fmt_i1

    ! Test internal READ
    string = '1234'
    read (string, fmt=100) ival
    print *, 'ival =', ival
    print *, 'internal READ: ', merge ('passed', 'failed', ival == 1234)

    ! Test internal WRITE
    ival = -42
    write (string, fmt=100) ival
    print *, 'string = ', trim (string)
    print *, 'internal WRITE: ', merge ('passed', 'failed', adjustl (string) == '-42')

    100 format (i4)

    ! ADD THE NEW ASSIGNED FORMAT TEST HERE
    assign 10 to fmt_i1
    10 format ("Assigned format WRITE: passed")
    write (*, fmt_i1)

end program