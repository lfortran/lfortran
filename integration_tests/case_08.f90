program case_08
    implicit none
    character(len=100) :: line
    character :: ch

    line = "-5"

    if (len_trim(line) < 2) then
        print *, "Error: Input line is too short: '", trim(line), "'"
        error stop
    end if

    ch = line(2:2)

    if (iachar(ch) < 32 .or. iachar(ch) > 126) then
        print *, "Error: Second character is non-printable (char code:", iachar(ch), ")"
        error stop
    end if

    select case (ch)
    case ("-", "0":"9")
        print *, "Second character is a '-' or a digit: ", ch
    case default
        print *, "Error: Unexpected second character: ", ch
        error stop
    end select

end program case_08
