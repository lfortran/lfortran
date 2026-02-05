! Test format string variable with trailing spaces (issue #4056)
! The bug was that trailing spaces in format string caused parsing error
program format_59
    implicit none
    character(12) :: fmt = "(2(1X,A))"
    character(80) :: output

    ! fmt is "(2(1X,A))   " with 3 trailing spaces - this was the bug
    write(output, fmt) 'Hello', 'world!'
    if (trim(adjustl(output)) /= 'Hello world!') error stop
end program format_59
