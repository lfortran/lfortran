program format_69
    implicit none
    integer :: i, u
    character :: c
    i = 69
    open(newunit=u, file="format_69.txt", status="replace", action="write")
    write(u, '(a)') i
    close(u)
    open(newunit=u, file="format_69.txt", action="read", status="old")
    read(u, '(a1)') c
    close(u, status="delete")
    if (c /= 'E') error stop "ASCII mismatch"
end program format_69