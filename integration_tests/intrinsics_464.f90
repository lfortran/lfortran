program intrinsics_463
    implicit none
    
    integer :: a(2, 3)
    integer :: r(2, 3)
    integer :: shifts(3)
    real :: input_array(2, 0)
    real :: result_array(2, 0)
    integer :: zero_shifts(0)

    a = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    shifts = [1, 0, -1]
    r = cshift(a, shifts)

    if (any(r /= reshape([2, 1, 3, 4, 6, 5], [2, 3]))) error stop

    result_array = cshift(input_array, zero_shifts)

    if (size(result_array) /= 0) error stop
end program