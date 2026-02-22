program format_61
    implicit none
    character(len=100) :: output

    write(output, '(g0)') 1.0
    if (trim(output) /= '1.00000000') error stop
    write(output, '(g0)') 0.0
    if (trim(output) /= '0.00000000') error stop
    write(output, '(g0)') -1.0
    if (trim(output) /= '-1.00000000') error stop
    write(output, '(g0)') 100000.0
    if (trim(output) /= '100000.000') error stop

    write(output, '(g0)') 1.0d0
    if (trim(output) /= '1.0000000000000000') error stop
    write(output, '(g0)') 0.0d0
    if (trim(output) /= '0.0000000000000000') error stop

    write(output, '(g0)') 42
    if (trim(output) /= '42') error stop
end program format_61
