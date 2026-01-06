program format_47
    implicit none
    integer :: i1, i2
    character(8) :: cdata

    cdata = '1   1   '
    read (cdata, 100) i1, i2
    if (i1 /= 1) error stop
    if (i2 /= 1000) error stop

100 format (bn, i4, bz, i4)
end program format_47