! Test for https://github.com/lfortran/lfortran/issues/4056
! Format string variable with trailing spaces (character(12))
! Exact MRE from issue comment (corrected version with character(12))
program format_59
    implicit none
    character(12) :: fmt = "(2(1X,A))"
    character(80) :: output
    print "(A)", 'fmt = "' // fmt // '"'
    print fmt, 'Hello', 'world!'
    write(output, fmt) 'Hello', 'world!'
    if (trim(adjustl(output)) /= 'Hello world!') error stop
end program format_59
