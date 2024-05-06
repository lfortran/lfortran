program iostat_constant_integer
    implicit none
    integer :: u = 10
    integer, parameter :: ios = 1

    open(u, file='tmp.txt')

    read(u, *, iostat=ios)
    close(u)
end program iostat_constant_integer
