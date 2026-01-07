program read_16
    ! Test ERR= label for read errors
    implicit none
    integer :: x, u
    character(len=20) :: invalid_data

    invalid_data = "not_an_integer"

    open(newunit=u, status='scratch')
    write(u, '(A)') invalid_data
    rewind(u)

    read(u, *, err=20) x
    error stop "Should have jumped to label 20"

20  continue
    close(u)
    print *, "PASS: ERR= label works"
end program read_16
