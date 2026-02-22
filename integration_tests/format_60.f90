program format_60
    implicit none
    character(4) :: s1, s2, s3

    open (10, file='format_60_tmp.txt', form='formatted', status='unknown')
    write (10, '(a)') 'abcd'
    write (10, '(a)') 'wxyz1234'
    
    rewind (10)
    s1 = '(no)'; s2 = '(no)'; s3 = '(no)'
    read  (10, '(a4/2a4)') s1, s2, s3
    if (s1 /= 'abcd') error stop
    if (s2 /= 'wxyz') error stop
    if (s3 /= '1234') error stop

    close (10, status='delete')
end program format_60
