program intrinsics_98
    integer :: a(3)
    real, parameter :: sum_ = sum([1, 2, 3])
    real, parameter :: size_ = size([1, 2, 3])
    real, parameter :: maxval_ = maxval([1, 2, 3])
    real, parameter :: minval_ = minval([1, 2, 3])
    a = [1, 2, 3]

    print *, shape(a)
    print *, size(a)
    if (size(a) /= 3) error stop
    print *, maxval(a)
    if (maxval(a) /= 3) error stop
    print *, minval(a)
    if (minval(a) /= 1) error stop
    print *, sum(a)
    if (sum(a) /= 6) error stop

    print*, sum_
    if (sum_ /= 6) error stop
    print*, size_
    if (size_ /= 3) error stop
    print*, maxval_
    if (maxval_ /= 3) error stop
    print*, minval_
    if (minval_ /= 1) error stop
    
end program
    