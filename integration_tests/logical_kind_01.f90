program logical_kind_01
    use iso_fortran_env, only: int8, int16, int32, int64
    implicit none
    logical(int8)  :: a8
    logical(int16) :: a16
    logical(int32) :: a32
    logical(int64) :: a64
    integer :: sz

    a8  = .true.
    a16 = .true.
    a32 = .true.
    a64 = .true.

    ! Verify logical kinds have correct storage sizes via unformatted stream I/O
    open(unit=10, file='logical_kind_01.tmp', status='replace', &
         action='write', access='stream')
    write(10) a8
    close(10)
    inquire(file='logical_kind_01.tmp', size=sz)
    if (sz /= 1) error stop

    open(unit=10, file='logical_kind_01.tmp', status='replace', &
         action='write', access='stream')
    write(10) a16
    close(10)
    inquire(file='logical_kind_01.tmp', size=sz)
    if (sz /= 2) error stop

    open(unit=10, file='logical_kind_01.tmp', status='replace', &
         action='write', access='stream')
    write(10) a32
    close(10)
    inquire(file='logical_kind_01.tmp', size=sz)
    if (sz /= 4) error stop

    open(unit=10, file='logical_kind_01.tmp', status='replace', &
         action='write', access='stream')
    write(10) a64
    close(10)
    inquire(file='logical_kind_01.tmp', size=sz)
    if (sz /= 8) error stop

    ! Verify assignment between different logical kinds
    a8  = .false.
    a16 = .false.
    a32 = .false.
    a64 = .false.

    if (a8  .neqv. .false.) error stop
    if (a16 .neqv. .false.) error stop
    if (a32 .neqv. .false.) error stop
    if (a64 .neqv. .false.) error stop

    ! Clean up
    open(unit=10, file='logical_kind_01.tmp', status='old')
    close(10, status='delete')

    print *, "All tests passed."
end program
