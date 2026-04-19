program file_63
    ! Test: unformatted sequential I/O with array records
    ! Verifies that reading back records after an array record works correctly.
    ! The trailing record marker must be consumed so subsequent reads succeed.
    use iso_fortran_env, only: int32, int64
    implicit none
    integer(int32) :: a, c, ra, rc
    integer(int64) :: b(1), rb(1)
    integer(int32) :: d(3), rd(3)
    integer :: u, ierr

    a = 1; b(1) = 2_int64; c = 3
    d = [10, 20, 30]

    open(newunit=u, form='unformatted', status='scratch', action='readwrite')
    write(u) a
    write(u) b
    write(u) c
    write(u) d
    rewind(u)
    read(u, iostat=ierr) ra
    if (ierr /= 0) error stop
    read(u, iostat=ierr) rb
    if (ierr /= 0) error stop
    read(u, iostat=ierr) rc
    if (ierr /= 0) error stop
    read(u, iostat=ierr) rd
    if (ierr /= 0) error stop
    close(u)

    if (ra /= 1) error stop
    if (rb(1) /= 2_int64) error stop
    if (rc /= 3) error stop
    if (rd(1) /= 10) error stop
    if (rd(2) /= 20) error stop
    if (rd(3) /= 30) error stop

    print *, "PASSED"
end program
