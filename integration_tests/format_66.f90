program format_66
    implicit none
    integer :: io, n
    character(len=80) :: line
    open(newunit=io, file="format_66_output.txt", status="replace", action="write")

    write(io, '("line1"/)')
    write(io, '("line2"//)')
    write(io, '("line3"///)')
    write(io, '(i1,/,i1)') 4, 5
    write(io, '("end")')
    close(io)
    open(newunit=io, file="format_66_output.txt", status="old", action="read")

    read(io, '(A)') line
    print *, line
    if (trim(line) /= "line1") error stop

    read(io, '(A)') line
    print *, line
    if (trim(line) /= "") error stop

    read(io, '(A)') line
    print *, line
    if (trim(line) /= "line2") error stop

    read(io, '(A)') line
    print *, line
    if (trim(line) /= "") error stop

    read(io, '(A)') line
    print *, line
    if (trim(line) /= "") error stop

    read(io, '(A)') line
    print *, line
    if (trim(line) /= "line3") error stop

    read(io, '(A)') line
    print *, line
    if (trim(line) /= "") error stop

    read(io, '(A)') line
    print *, line
    if (trim(line) /= "") error stop

    read(io, '(A)') line
    print *, line
    if (trim(line) /= "") error stop

    read(io, '(A)') line
    print *, line
    if (trim(line) /= "4") error stop

    read(io, '(A)') line
    print *, line
    if (trim(line) /= "5") error stop

    read(io, '(A)') line
    print *, line
    if (trim(line) /= "end") error stop

    close(io, status="delete")
end program format_66
