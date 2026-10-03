program format_106
    implicit none
    character(len=16) :: lines(4)

    lines = "sentinel"
    write(lines, '(A,2/,A)') 'x', 'y'
    if (lines(1) /= 'x') error stop
    if (len_trim(lines(2)) /= 0) error stop
    if (lines(3) /= 'y') error stop
    if (lines(4) /= 'sentinel') error stop

    lines = "sentinel"
    write(lines, '(2/,A)') 'z'
    if (len_trim(lines(1)) /= 0) error stop
    if (len_trim(lines(2)) /= 0) error stop
    if (lines(3) /= 'z') error stop
    if (lines(4) /= 'sentinel') error stop
end program format_106
