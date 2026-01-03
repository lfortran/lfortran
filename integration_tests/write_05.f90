program write_05
    implicit none

    character(24) :: s
    integer :: x1 = 12
    integer :: x2 = 34
    write(s, *) x1, x2
    ! TODO Handle String formatting
    ! print *, "_", s, "_", x1, x2
    ! if (trim(s) /= "1234") error stop
    x1 = x1 * x2
    x2 = x1 + x2
    write(s, "(i0, i0)") x1, x2
    ! TODO Handle string length
    ! Check with GFortran to see the difference
    ! if (len(s) /= 24) error stop
    if (trim(s) /= "408442") error stop

end program write_05
