program file_29
    implicit none
    integer :: io, stat
    character(len=31) :: buffer
    character(len=3) :: temp = "no"

    open(newunit=io, status="scratch")
    write(io, "(a)") repeat("abc", 10), repeat("def", 100), repeat("ghi", 1000)
    rewind(io)

    ! Test 1: advance="no" triggers EOR
    read(io, '(a)', advance="no", iostat=stat) buffer
    if (stat /= -2) error stop
    if (.not. is_iostat_eor(stat)) error stop
    if (is_iostat_end(stat)) error stop

    ! Test 2: normal read after EOR
    read(io, '(a)', advance="no", iostat=stat) buffer
    if (stat /= 0) error stop
    if (is_iostat_eor(stat)) error stop
    if (is_iostat_end(stat)) error stop

    ! Test 3: advance="yes" normal read
    rewind(io)
    read(io, '(a)', advance="yes", iostat=stat) buffer
    if (stat /= 0) error stop
    if (is_iostat_eor(stat)) error stop
    if (is_iostat_end(stat)) error stop

    ! Test 4: variable advance mode ("no")
    rewind(io)
    read(io, '(a)', advance=temp, iostat=stat) buffer
    if (stat /= -2) error stop
    if (.not. is_iostat_eor(stat)) error stop
    if (is_iostat_end(stat)) error stop

    ! Test 5: switch temp to "yes" and continue
    temp = "yes"
    read(io, '(a)', advance=temp, iostat=stat) buffer
    if (stat /= 0) error stop

    ! Check end of file (EOF)
    do
        read(io, '(a)', iostat=stat) buffer
        if (stat /= 0) exit
    end do

    if (.not. is_iostat_end(stat)) error stop

    close(io)
end program file_29
