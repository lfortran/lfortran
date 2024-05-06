program iostat_constant_integer
    implicit none
    integer, parameter :: ios = 1
    character(len=100) :: buffer
    buffer = 'Temporary date for testing purpose'

    read(buffer, *, iostat=ios)
end program iostat_constant_integer
