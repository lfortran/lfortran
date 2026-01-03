program intrinsics_43
    implicit none
    integer :: x = dim(4, 15)
    integer :: y = dim(15, 4)
    real(8) :: p = dim(4.345_8, 2.111_8)
    real(8) :: q = dim(2.111_8, 4.345_8)
    
    if (x /= 0) error stop
    if (y /= 11) error stop
    if (p /= (4.345_8 - 2.111_8)) error stop
    if (q /= 0) error stop

    print *, x, y, p, q
end program
