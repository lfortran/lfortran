program stdin_read_advance_01
    implicit none

    character(len=1) :: c
    integer :: n, ios

    ! An empty formatted READ with ADVANCE='no' must leave the record
    ! open, so the next read still starts on the same record. Before the
    ! fix the record-advance hook ignored ADVANCE= and skipped past it.
    ! (File-unit counterpart: integration_tests/read_100.f90.)
    read(*, '(a)', advance='no')
    read(*, *, iostat=ios) n
    if (ios /= 0) error stop 1
    if (n /= 42) error stop 2

    ! The trailing text of the non-advancing record is discarded by the
    ! list-directed advance, so the next read starts on the next record.
    read(*, '(a1)', iostat=ios) c
    if (ios /= 0) error stop 3
    if (c /= 'A') error stop 4

end program stdin_read_advance_01
