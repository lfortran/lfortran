program allocate_65
    implicit none
    real(8), allocatable :: mean_
    real(8) :: mean_val
    mean_val = 3.14_8
    allocate(mean_, source=mean_val)
    if (.not. allocated(mean_)) error stop
    if (abs(mean_ - mean_val) > 1.0e-10_8) error stop
end program allocate_65