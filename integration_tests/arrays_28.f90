program arrays_28
    implicit none
    integer :: dim = 1, i, size = 6
    integer :: arr_01(6) = [-14, 3, 0, -2, 19, 1]
    real(8), allocatable :: arr_02(:)

    allocate(arr_02(size))

    do i = 1, size
        arr_02(i) = arr_01(size - i + 1)
    end do

    if (maxloc(arr_01, dim) /= 5) error stop
    if (maxloc(arr_02, dim=1) /= 2) error stop
end program arrays_28
