! Test format string variable with trailing spaces (issue #4056)
program format_54
    implicit none
    character(9) :: fmt = "(2(1X,A))"
    character(80) :: output

    write(output, fmt) 'Hello', 'world!'
    if (trim(adjustl(output)) /= 'Hello world!') error stop
end program format_54
