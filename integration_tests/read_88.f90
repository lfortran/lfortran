program read_88
    implicit none
    character(256) :: val
    integer, dimension(:), allocatable :: dyn
    integer, dimension(2) :: padding
 
    allocate(dyn(2))
    val = "10 20"
    read(val, *) dyn
 
    val = "1 2"
    read(val, *) padding
 
    if (dyn(1) /= 10) error stop "bad dyn(1)"
    if (dyn(2) /= 20) error stop "bad dyn(2)"
    if (padding(1) /= 1) error stop "bad padding(1)"
    if (padding(2) /= 2) error stop "bad padding(2)"
 end program read_88