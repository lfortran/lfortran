program file_75
    ! An empty non-advancing WRITE still opens a record. Completing or
    ! repositioning the record (an advancing WRITE, CLOSE, ENDFILE, BACKSPACE,
    ! REWIND) terminates it: the data a preceding WRITE had written over is
    ! gone, and the empty record can be read back.
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

    ! CLOSE terminates the record a non-advancing WRITE left open, so appending
    ! after reopening starts a new record instead of gluing data onto it.
    open(newunit=u, file='file_75_d.txt', status='replace')
    write(u, '(a)', advance='no') 'ab'
    close(u)
    open(newunit=u, file='file_75_d.txt', status='old', position='append')
    write(u, '(a)') 'cd'
    rewind(u)
    read(u, '(a)', iostat=ios) s
    if (ios /= 0) error stop 'CLOSE did not terminate the record'
    if (trim(s) /= 'ab') error stop 'CLOSE glued data onto the pending record'
    read(u, '(a)', iostat=ios) s
    if (ios /= 0) error stop 'record appended after CLOSE is missing'
    if (trim(s) /= 'cd') error stop 'wrong record appended after CLOSE'
    read(u, '(a)', iostat=ios) s
    if (ios == 0) error stop 'unexpected record after CLOSE'
    close(u, status='delete')

    ! ENDFILE terminates the record a non-advancing WRITE left open.
    open(newunit=u, file='file_75_e.txt', status='replace')
    write(u, '(a)', advance='no') 'ab'
    endfile(u)
    close(u)
    open(newunit=u, file='file_75_e.txt', status='old', position='append')
    write(u, '(a)') 'cd'
    rewind(u)
    read(u, '(a)', iostat=ios) s
    if (ios /= 0) error stop 'ENDFILE did not terminate the record'
    if (trim(s) /= 'ab') error stop 'ENDFILE glued data onto the pending record'
    read(u, '(a)', iostat=ios) s
    if (ios /= 0) error stop 'record appended after ENDFILE is missing'
    if (trim(s) /= 'cd') error stop 'wrong record appended after ENDFILE'
    read(u, '(a)', iostat=ios) s
    if (ios == 0) error stop 'unexpected record after ENDFILE'
    close(u, status='delete')

    ! BACKSPACE terminates the record a non-advancing WRITE left open.
    open(newunit=u, file='file_75_f.txt', status='replace')
    write(u, '(a)', advance='no') 'ab'
    backspace(u)
    close(u)
    open(newunit=u, file='file_75_f.txt', status='old', position='append')
    write(u, '(a)') 'cd'
    rewind(u)
    read(u, '(a)', iostat=ios) s
    if (ios /= 0) error stop 'BACKSPACE did not terminate the record'
    if (trim(s) /= 'ab') error stop 'BACKSPACE glued data onto the pending record'
    read(u, '(a)', iostat=ios) s
    if (ios /= 0) error stop 'record appended after BACKSPACE is missing'
    if (trim(s) /= 'cd') error stop 'wrong record appended after BACKSPACE'
    read(u, '(a)', iostat=ios) s
    if (ios == 0) error stop 'unexpected record after BACKSPACE'
    close(u, status='delete')

    ! An advancing WRITE with no output list completes the open record.
    open(newunit=u, file='file_75_g.txt', status='replace')
    write(u, '(a)', advance='no') 'ab'
    write(u, '(a)')
    rewind(u)
    read(u, '(a)', iostat=ios) s
    if (ios /= 0) error stop 'empty advancing WRITE lost the record'
    if (trim(s) /= 'ab') error stop 'empty advancing WRITE changed the data'
    read(u, '(a)', iostat=ios) s
    if (ios == 0) error stop 'empty advancing WRITE opened another record'
    close(u, status='delete')

    ! A non-advancing WRITE with no output list opens an empty record.
    open(newunit=u, file='file_75_h.txt', status='replace')
    write(u, '(a)') 'cza'
    write(u, '(a)', advance='no')
    rewind(u)
    read(u, '(a)', iostat=ios) s
    if (ios /= 0) error stop 'record before empty WRITE changed'
    if (trim(s) /= 'cza') error stop 'wrong record before empty WRITE'
    read(u, '(a)', iostat=ios) s
    if (ios /= 0) error stop 'empty WRITE with no list created no record'
    if (len_trim(s) /= 0) error stop 'empty listless WRITE record is not empty'
    read(u, '(a)', iostat=ios) s
    if (ios == 0) error stop 'unexpected record after empty WRITE'
    close(u, status='delete')
end program file_75
