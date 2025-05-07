program inquire_02
    integer :: unit, pos_value
    character(len=1) :: ch

    open(unit=10, file="data.txt")
    write(10, '(A)', advance='no') 'abcd'  ! writes 4 characters, no newline
    rewind(10)                            ! go back to start
    read(10, '(A)', advance='no') ch      ! read 1 character (1 byte)
    inquire(unit=10, pos=pos_value)
    print *, "Position:", pos_value
    if(pos_value /= 2) error stop
end program