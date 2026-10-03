program read_100
    ! An empty formatted READ with ADVANCE='no' must leave the record
    ! open, so the next READ continues the same record. Any other
    ! ADVANCE= (or none) advances to the next record as before.
    implicit none
    integer :: u, n, m, ios
    character(len=1) :: c1, c2
    character(len=2) :: adv
    character(len=10) :: adv10

    open(newunit=u, file='read_100.txt', status='replace')
    write(u, '(i0)') 42
    write(u, '(i0)') 77

    ! ADVANCE='no' on an empty read leaves the record open.
    rewind(u)
    read(u, '(a)', advance='no')
    read(u, *, iostat=ios) n
    if (ios /= 0) error stop 1
    if (n /= 42) error stop 2

    ! ADVANCE='yes' advances like the default.
    rewind(u)
    read(u, '(a)', advance='yes')
    read(u, *, iostat=ios) n
    if (ios /= 0) error stop 3
    if (n /= 77) error stop 4

    ! No ADVANCE= advances as before.
    rewind(u)
    read(u, '(a)')
    read(u, *, iostat=ios) n
    if (ios /= 0) error stop 5
    if (n /= 77) error stop 6

    ! Matching is case-insensitive.
    rewind(u)
    read(u, '(a)', advance='NO')
    read(u, *, iostat=ios) n
    if (ios /= 0) error stop 7
    if (n /= 42) error stop 8

    ! A variable ADVANCE= behaves like the literal.
    adv = 'no'
    rewind(u)
    read(u, '(a)', advance=adv)
    read(u, *, iostat=ios) n
    if (ios /= 0) error stop 9
    if (n /= 42) error stop 10

    ! A blank-padded variable ADVANCE= behaves like the literal too:
    ! the frontend trims it, so the record stays open.
    adv10 = 'no'
    rewind(u)
    read(u, '(a)', advance=adv10)
    read(u, *, iostat=ios) n
    if (ios /= 0) error stop 11
    if (n /= 42) error stop 12
    close(u, status='delete')

    ! Nonadvancing value reads still complete one record (unchanged path).
    open(newunit=u, file='read_100.txt', status='replace')
    write(u, '(i2,i2)') 42, 77
    rewind(u)
    read(u, '(i2)', advance='no', iostat=ios) n
    if (ios /= 0) error stop 13
    if (n /= 42) error stop 14
    read(u, '(i2)', iostat=ios) m
    if (ios /= 0) error stop 15
    if (m /= 77) error stop 16

    ! Consecutive nonadvancing reads advance within the same record.
    rewind(u)
    write(u, '(i2)') 42
    rewind(u)
    read(u, '(a1)', advance='no', iostat=ios) c1
    if (ios /= 0) error stop 17
    if (c1 /= '4') error stop 18
    read(u, '(a1)', advance='no', iostat=ios) c2
    if (ios /= 0) error stop 19
    if (c2 /= '2') error stop 20
    close(u, status='delete')
end program read_100
