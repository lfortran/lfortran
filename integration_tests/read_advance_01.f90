program read_advance_01
    implicit none
    integer :: u, n, m, ios
    character(len=3) :: advance_spec
    character(len=1) :: ch
    character(len=1) :: ch2

    open (newunit=u, file='read_advance_01.txt', status='replace')
    write (u, '(i0)') 42
    write (u, '(i0)') 77
    rewind (u)
    read (u, '(a)', advance='no')      ! ADVANCE='no' must leave the record open
    read (u, *, iostat=ios) n
    if (ios /= 0 .or. n /= 42) error stop "ADVANCE='no' skipped the record"
    read (u, *, iostat=ios) m
    if (ios /= 0 .or. m /= 77) error stop "next record wrong"
    close (u)

    open (newunit=u, file='read_advance_01.txt', status='old')
    read (u, *, iostat=ios) n          ! list read must advance to the next record
    if (ios /= 0 .or. n /= 42) error stop "list read wrong"
    read (u, '(a)', iostat=ios)        ! empty read with default ADVANCE='yes' skips
    read (u, *, iostat=ios) m
    if (ios >= 0) error stop "expected end of file"
    close (u, status='delete')

    ! A blank-padded ADVANCE='no' (a character(3) variable) must behave like
    ! the literal: trailing blanks compare equal, so the record stays open.
    open (newunit=u, file='read_advance_01.txt', status='replace')
    write (u, '(i0)') 42
    rewind (u)
    advance_spec = 'no'
    read (u, '(a)', advance=advance_spec)
    read (u, '(a1)', iostat=ios) ch
    if (ios /= 0 .or. ch /= '4') error stop "padded ADVANCE='no' skipped the record"
    close (u, status='delete')

    ! ADVANCE='no' with a non-empty input list: the record stays open, so a
    ! second read of the same statement kind completes it (empty_read must
    ! skip nothing after a successful value read).
    open (newunit=u, file='read_advance_01.txt', status='replace')
    write (u, '(i2,i2)') 42, 77
    rewind (u)
    read (u, '(i2)', advance='no', iostat=ios) n
    if (ios /= 0 .or. n /= 42) error stop "advance=no value read wrong"
    read (u, '(i2)', iostat=ios) m
    if (ios /= 0 .or. m /= 77) error stop "second read did not complete the record"
    close (u, status='delete')

    ! Lowercase matching is case-insensitive: 'NO' behaves like 'no'.
    open (newunit=u, file='read_advance_01.txt', status='replace')
    write (u, '(i0)') 42
    rewind (u)
    read (u, '(a)', advance='NO')
    read (u, '(a1)', iostat=ios) ch
    if (ios /= 0 .or. ch /= '4') error stop "uppercase advance='NO' skipped the record"
    close (u, status='delete')

    ! Explicit ADVANCE='yes' advances like the default.
    open (newunit=u, file='read_advance_01.txt', status='replace')
    write (u, '(i0)') 42
    write (u, '(i0)') 77
    rewind (u)
    read (u, '(a)', advance='yes')
    read (u, *, iostat=ios) n
    if (ios /= 0 .or. n /= 77) error stop "advance='yes' did not skip the record"
    close (u, status='delete')

    ! Consecutive nonadvancing reads advance within the same record.
    open (newunit=u, file='read_advance_01.txt', status='replace')
    write (u, '(i2)') 42
    rewind (u)
    read (u, '(a1)', advance='no', iostat=ios) ch
    if (ios /= 0 .or. ch /= '4') error stop "first nonadvancing read wrong"
    read (u, '(a1)', advance='no', iostat=ios) ch2
    if (ios /= 0 .or. ch2 /= '2') error stop "second nonadvancing read wrong"
    close (u, status='delete')

    print *, 'ok'
end program
