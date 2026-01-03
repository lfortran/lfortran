program iostat_non_scalar_value
    implicit none
    integer :: ios(2) = 1
    character(len=100) :: buffer
    buffer = 'Temporary date for testing purpose'

    read(buffer, *, iostat=ios(1:1))
end program iostat_non_scalar_value
