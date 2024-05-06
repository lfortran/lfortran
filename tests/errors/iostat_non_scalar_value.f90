program iostat_non_scalar_value
    implicit none
    integer :: u = 10
    integer :: ios(2)

    open(u, file='tmp.txt')

    read(u, *, iostat=ios(1:1))
    close(u)
end program iostat_non_scalar_value
