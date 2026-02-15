program where_17
    implicit none
    integer :: nz(3), maxnz(3), minnz(3)
    integer :: mx

    nz = [480, 120, 1]

    maxnz = 0
    where (nz > 1)
        maxnz = maxval(nz)
    end where
    if (maxnz(1) /= 480) error stop
    if (maxnz(2) /= 480) error stop
    if (maxnz(3) /= 0) error stop

    minnz = 0
    where (nz > 100)
        minnz = minval(nz)
    end where
    if (minnz(1) /= 1) error stop
    if (minnz(2) /= 1) error stop
    if (minnz(3) /= 0) error stop

    mx = maxnz(1)
    if (mx /= 480) error stop

    print *, mx
end program where_17
