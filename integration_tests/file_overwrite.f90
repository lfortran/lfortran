program overwrite_with_rec
    use iso_fortran_env, only: int32
    implicit none

    integer(int32) :: x, y ,z

    x = 654321
    z = 456465
    open(10, file="file_overwrite.bin", access="direct", recl=4)
    write(10, rec=1) x
    write(10,rec=1) z
    close(10)

    open(10, file="file_overwrite.bin", access="direct", recl=4)
    read(10, rec=1) y
    close(10)
    if (y /= z) error stop 
end program
