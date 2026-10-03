program intrinsics_415
    real(4) :: x(5) = [1.0, 2.0, 3.0, 4.0, 5.0]
    integer :: y(5) = [1, 2, 3, 4, 5]

    if (abs(sum(x, mask = .true.) - 15.0) > 1e-6) error stop
    if (sum(y, mask = .true.) /= 15) error stop
    if (abs(product(x, mask = .true.) - 120.0) > 1e-6) error stop
    if (product(y, mask = .true.) /= 120) error stop
end program
