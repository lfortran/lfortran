program list_directed_stdin_eof_01
    implicit none
    character(len=1) :: key
    character(len=10) :: word
    integer :: ios, u

    u = 20
    open(u, file="list_directed_stdin_eof_01_data.txt", status="old")

    ! Test 1: Read a single character via list-directed I/O
    read(u, *) key
    if (key /= "A") error stop "Test 1 failed: expected 'A'"

    ! Test 2: Read another character (should skip the newline properly)
    read(u, *) key
    if (key /= "B") error stop "Test 2 failed: expected 'B'"

    ! Test 3: Read a word
    read(u, *) word
    if (trim(word) /= "Hello") error stop "Test 3 failed: expected 'Hello'"

    ! Test 4: Read past end-of-file with iostat
    read(u, *, iostat=ios) key
    if (ios >= 0) error stop "Test 4 failed: expected EOF (negative iostat)"

    close(u)

    print *, "All list-directed character read tests passed."
end program
