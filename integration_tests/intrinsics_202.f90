program intrinsics_202
    integer, parameter :: dp = kind(0.d0)
    real(dp), parameter :: A(*) = [1._dp, 2._dp, 3._dp]
    real(dp), parameter :: B(*) = A
    real(dp), parameter :: C(*) = sin(A)

    print *, B
    if (any(A /= B)) error stop

    print *, C
    if (abs(C(1) - 8.41470984807896505e-01) > 1e-5) error stop
    if (abs(C(2) - 9.09297426825681727e-01) > 1e-5) error stop
    if (abs(C(3) - 0.14112000805986721) > 1e-5) error stop
end program
