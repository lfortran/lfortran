program intrinsics_447
    implicit none
    real(8) :: x(2, 2)
    real :: s(2)
    x = reshape([1.0_8, 2.0_8, 3.0_8, 4.0_8], [2, 2])
    s = sum(sngl(x), 1) / 1.0
    if (abs(s(1) - 3.0) > 1.0e-5) error stop
    if (abs(s(2) - 7.0) > 1.0e-5) error stop
    print *, s
end program intrinsics_447
