program allocate_79
    ! allocate(x, source=y) of a rank-1 array without a shape
    ! specification, into an array that was allocated and deallocated before
    implicit none
    real, allocatable :: mean_(:)
    real, allocatable :: tmp(:)
    integer :: i

    allocate(mean_(10))
    mean_ = [(real(i), i = 1, 10)]
    allocate(tmp(10))
    deallocate(tmp)
    allocate(tmp, source=mean_)
    print *, allocated(tmp)
    print *, tmp
    if (.not. allocated(tmp)) error stop
    if (size(tmp) /= 10) error stop
    if (lbound(tmp, 1) /= 1 .or. ubound(tmp, 1) /= 10) error stop
    if (any(tmp /= mean_)) error stop

    deallocate(tmp, mean_)
    allocate(mean_(-5:4994))
    mean_ = [(real(i), i = -5, 4994)]
    allocate(tmp(3))
    deallocate(tmp)
    allocate(tmp, source=mean_)
    if (size(tmp) /= 5000) error stop
    if (lbound(tmp, 1) /= -5 .or. ubound(tmp, 1) /= 4994) error stop
    if (any(tmp /= mean_)) error stop
    if (tmp(4994) /= 4994.0) error stop
end program allocate_79
