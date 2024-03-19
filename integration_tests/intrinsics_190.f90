program intrinsics_190
    implicit none
    integer :: res(1)
    print *, kind([real:: 1]) ! this line is necessary to assure it works
    res = kind([real:: 1])
    print *, res
    if (res(1) /= 4) error stop
end program
