program format_62
    implicit none

    character(16) :: answer
    real :: ranswer

    open (10, file='format_62_tmp.txt', status='unknown', form='formatted')
    write (10, '(f5.4)') 0.9999

    rewind (10)
    read (10, '(a)') answer
    if (trim(answer) /= '.9999') error stop

    rewind (10)
    write (10, '(a)') '.9999'

    rewind (10)
    read (10, '(f5.4)') ranswer
    if (abs(ranswer - 0.9999) >= 0.0001) error stop

    rewind (10)
    write (10, '(f6.4)') 0.9999

    rewind (10)
    read (10, '(a)') answer
    if (trim(answer) /= '0.9999') error stop

    rewind (10)
    write (10, '(f4.3)') 0.123

    rewind (10)
    read (10, '(a)') answer
    if (trim(answer) /= '.123') error stop

    rewind (10)
    write (10, '(f5.3)') 0.123

    rewind (10)
    read (10, '(a)') answer
    if (trim(answer) /= '0.123') error stop

    rewind (10)
    write (10, '(f5.3)') -0.123

    rewind (10)
    read (10, '(a)') answer
    if (trim(answer) /= '-.123') error stop

    close (10, status='delete')

end program format_62
