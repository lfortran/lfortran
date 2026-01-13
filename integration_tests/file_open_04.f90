program open_stream_form
    use iso_fortran_env, only: int32
    implicit none

    integer(int32) :: x, y

    x = 123456

    open(10, file="file_open_04_data.bin", access="stream")
    write(10) x
    close(10)

    open(10, file="file_open_04_data.bin", access="stream")
    read(10) y
    close(10)

    if (y /= x) then
        print *, "FAIL: expected", x, "got", y
        error stop 1
    end if
end program
