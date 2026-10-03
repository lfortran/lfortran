program read_08
    ! Test reading logical values with T/F format
    implicit none
    integer :: unit_no
    logical :: val1, val2, val3, val4

    open(newunit=unit_no, status='scratch')
    write(unit_no, '(A)') 'T'
    write(unit_no, '(A)') 'F'
    write(unit_no, '(A)') 't'
    write(unit_no, '(A)') 'f'
    rewind(unit_no)

    read(unit_no, *) val1
    read(unit_no, *) val2
    read(unit_no, *) val3
    read(unit_no, *) val4
    close(unit_no)

    if (.not. val1) error stop "Expected T to be .true."
    if (val2) error stop "Expected F to be .false."
    if (.not. val3) error stop "Expected t to be .true."
    if (val4) error stop "Expected f to be .false."

    print *, "PASS"
end program
