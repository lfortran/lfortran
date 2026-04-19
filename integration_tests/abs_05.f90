program abs_05
    implicit none
    real, parameter :: a(2,2) = reshape([1.0, 2.0, 3.0, 4.0], [2,2])
    real :: b(2,2), c(2,2)
    b = 1.0
    c = b + abs(a)
    print *, c(1,1), c(2,1), c(1,2), c(2,2)
    if (nint(c(1,1)) /= 2) error stop
    if (nint(c(2,1)) /= 3) error stop
    if (nint(c(1,2)) /= 4) error stop
    if (nint(c(2,2)) /= 5) error stop
    print *, "PASS"
end program
