program file_75
    ! An empty non-advancing WRITE still opens a record. REPOSITIONING the
    ! unit (BACKSPACE, REWIND) terminates that record: the data a preceding
    ! WRITE had written over is gone, and the empty record can be read back.
    implicit none
    integer :: ios, u
    character(len=16) :: s

    ! After BACKSPACE the empty WRITE must delete the record it wrote over.
    open(newunit=u, file='file_75_a.txt', status='replace')
    write(u, '(a)') 'cza'
    write(u, '(a)') 'zczza'
    backspace(u)
    write(u, '(a)', advance='no') ''
    rewind(u)
    read(u, '(a)', iostat=ios) s
    if (ios /= 0) error stop 'first read failed'
    if (trim(s) /= 'cza') error stop 'record before BACKSPACE changed'
    read(u, '(a)', iostat=ios) s
    if (ios /= 0) error stop 'empty record was not created'
    if (len_trim(s) /= 0) error stop 'empty record is not empty'
    read(u, '(a)', iostat=ios) s
    if (ios == 0) error stop 'deleted record was read back'
    close(u, status='delete')

    ! Without BACKSPACE the pending empty record must also exist after REWIND.
    open(newunit=u, file='file_75_b.txt', status='replace')
    write(u, '(a)') 'cza'
    write(u, '(a)', advance='no') ''
    rewind(u)
    read(u, '(a)', iostat=ios) s
    if (ios /= 0) error stop 'first read failed'
    if (trim(s) /= 'cza') error stop 'wrong first record'
    read(u, '(a)', iostat=ios) s
    if (ios /= 0) error stop 'empty record was not created'
    if (len_trim(s) /= 0) error stop 'empty record is not empty'
    close(u, status='delete')

    ! An advancing WRITE still completes the record a non-advancing one opened.
    open(newunit=u, file='file_75_c.txt', status='replace')
    write(u, '(a)', advance='no') 'ab'
    write(u, '(a)', advance='no') ''
    write(u, '(a)') 'cd'
    rewind(u)
    read(u, '(a)', iostat=ios) s
    if (ios /= 0) error stop 'joined record not read'
    if (trim(s) /= 'abcd') error stop 'advancing WRITE started a new record'
    read(u, '(a)', iostat=ios) s
    if (ios == 0) error stop 'unexpected extra record'
    close(u, status='delete')
end program file_75
