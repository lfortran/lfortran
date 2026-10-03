program format_100
    implicit none
    character(len=200) :: buf
    integer :: u

    ! Test: X descriptor before / should not produce trailing blanks
    open(newunit=u, file="__format_95_tmp.txt", status="replace", action="write")
    write(u, '(A,3X,/,A)') "hello", "world"
    close(u)

    open(newunit=u, file="__format_95_tmp.txt", status="old", action="read")
    read(u, '(A)') buf
    if (len_trim(buf) /= 5) error stop
    read(u, '(A)') buf
    if (len_trim(buf) /= 5) error stop
    close(u, status="delete")

    ! Test: X at end of format should not produce trailing blanks
    open(newunit=u, file="__format_95_tmp.txt", status="replace", action="write")
    write(u, '(A,5X)') "test"
    close(u)

    open(newunit=u, file="__format_95_tmp.txt", status="old", action="read")
    read(u, '(A)') buf
    if (len_trim(buf) /= 4) error stop
    close(u, status="delete")

    print *, "ok"
end program
