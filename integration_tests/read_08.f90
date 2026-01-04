program read_08
    ! Test reading logical values with T/F format
    implicit none
    logical :: val1, val2, val3, val4

    open(10, status='scratch')
    write(10, '(A)') 'T'
    write(10, '(A)') 'F'
    write(10, '(A)') 't'
    write(10, '(A)') 'f'
    rewind(10)

    read(10, *) val1
    read(10, *) val2
    read(10, *) val3
    read(10, *) val4
    close(10)

    if (.not. val1) error stop "Expected T to be .true."
    if (val2) error stop "Expected F to be .false."
    if (.not. val3) error stop "Expected t to be .true."
    if (val4) error stop "Expected f to be .false."

    print *, "PASS"
end program
