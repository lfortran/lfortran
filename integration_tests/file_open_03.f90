program open_direct_form
    use iso_fortran_env, only: int32
    implicit none

    integer(int32) :: x, y

    x = 654321

    open(10, file="file_open_03_data.bin", access="direct", recl=4)
    write(10, rec=1) x
    close(10)

    open(10, file="file_open_03_data.bin", access="direct", recl=4)
    read(10, rec=1) y
    close(10)

    if (y /= x) then
        print *, "FAIL: expected", x, "got", y
        error stop 1
    end if
end program
