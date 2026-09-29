program stdin_read_list_then_format_01
    implicit none

    character(len=10) :: w
    character(len=1) :: c
    integer :: n, i, j, p, q, ios

    ! A list-directed read stops at the record terminator. The READ statement
    ! must still advance past it, otherwise the next format-directed read
    ! sees the leftover terminator and returns a blank, empty record.
    read(*, *) n
    if (n /= 42) error stop 1
    read(*, '(a1)') c
    if (c /= "A") error stop 2

    ! Same, with more than one value transferred by the list-directed read.
    read(*, *) i, j
    if (i /= 7) error stop 3
    if (j /= 8) error stop 4
    read(*, '(a1)') c
    if (c /= "B") error stop 5

    ! Whatever is left in the record after the last value is discarded by the
    ! advance, so the format-directed read starts at the next record.
    read(*, *) w
    if (w /= "zz") error stop 6
    read(*, '(a1)') c
    if (c /= "C") error stop 7

    ! A list-directed read with an empty input list advances one record.
    read(*, *, iostat=ios)
    if (ios /= 0) error stop 8
    read(*, '(a1)') c
    if (c /= "D") error stop 9

    ! Format-directed followed by list-directed keeps working.
    read(*, '(a1)') c
    if (c /= "E") error stop 10
    read(*, *) w
    if (w /= "hello") error stop 11

    ! List-directed followed by list-directed keeps working.
    read(*, *) p
    if (p /= 99) error stop 12
    read(*, *) q
    if (q /= 100) error stop 13

    ! The advance past the final record must not turn a successful read into
    ! an end-of-file condition.
    read(*, *, iostat=ios) n
    if (ios /= 0) error stop 14
    if (n /= 55) error stop 15

    ! Now the stream really is exhausted.
    read(*, '(a1)', iostat=ios) c
    if (ios >= 0) error stop 16

end program stdin_read_list_then_format_01
