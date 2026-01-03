program read_14
    use iso_fortran_env, only: int64
    implicit none

    integer(int64) :: x
    integer :: unit, ios

    open(newunit=unit, status="scratch", action="readwrite")
    write(unit, "(A)") "abc"
    rewind(unit)
    read(unit, *, iostat=ios) x
    close(unit)

    if (ios <= 0) error stop
end program read_14
