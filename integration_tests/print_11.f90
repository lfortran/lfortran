program print_11
    implicit none
    character(50) :: out

    write(out, *) '"', '?', '"'
    if (trim(adjustl(out)) /= '"?"') error stop

    write(out, *) 'A', 'B', 'C'
    if (trim(adjustl(out)) /= 'ABC') error stop
end program print_11
