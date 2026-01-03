program read_03
    integer(8) :: blocks(2)
    integer :: unit

    open(newunit=unit, file="read_03_data.txt")
    read(unit, *) blocks
    if (blocks(1) /= 123456789) error stop
    if (blocks(2) /= 987654321) error stop
    close(unit)

    print *, "Read text data:", blocks
end program read_03