program inquire_10
    use iso_fortran_env, only: int64
    implicit none
    integer(int64) :: file_size
    integer :: fh
    character(*), parameter :: fname = "inquire_10_test.txt"

    open(newunit=fh, file=fname, access="stream", form="unformatted", status="replace")
    write(fh) "hello"
    close(fh)

    inquire(file=fname, size=file_size)
    print *, "File size =", file_size, "bytes"
    if (file_size /= 5) error stop

    open(newunit=fh, file=fname, status="old")
    close(fh, status="delete")
end program inquire_10
