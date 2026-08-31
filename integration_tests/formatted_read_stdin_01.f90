program formatted_read_stdin_01
    implicit none
    character(len=1) :: c
    integer :: ios

    print '(a)', 'INPUT?'
    read (*, '(A1)', iostat=ios) c
    if (ios /= 0) error stop "read failed"
    if (c /= 'A') error stop "wrong character read"
    print '(2a)', 'GOT:', c
end program
