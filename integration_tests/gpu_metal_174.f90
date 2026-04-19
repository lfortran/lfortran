program gpu_metal_174
    ! Test: do concurrent with ubound() on a 0-based allocatable array.
    ! Verifies that the Metal kernel uses the correct upper bound (ubound)
    ! rather than the array extent (size) when the lower bound is not 1.
    implicit none
    integer, allocatable :: arr(:)
    integer :: results(4), i

    allocate(arr(0:3))
    arr = [10, 20, 30, 40]
    results = 0

    do concurrent(i = 1:ubound(arr,1))
        results(i) = 1
    end do

    if (results(1) /= 1) error stop
    if (results(2) /= 1) error stop
    if (results(3) /= 1) error stop
    if (results(4) /= 0) error stop
    print *, "PASS"
end program
