! Test complex abs() with large values (near overflow)
! This tests the numerically stable algorithm for complex abs
program intrinsics_407
    implicit none
    complex :: c1, c2, c3, c4
    complex(8) :: z1, z2
    real :: r1, r2, r3, r4
    real(8) :: d1, d2

    ! Normal case - basic sanity check
    c1 = (3.0, 4.0)
    r1 = abs(c1)
    if (abs(r1 - 5.0) > 1e-5) error stop "Test 1 failed"

    ! Large values (near overflow for single precision)
    ! sqrt(5.07e30^2 + 2.54e30^2) would overflow naively
    ! but the correct result ~5.67e30 is representable
    c2 = (5.07e30, 2.54e30)
    r2 = abs(c2)
    if (r2 < 5.0e30 .or. r2 > 6.0e30) error stop "Test 2 failed: overflow in abs()"

    ! Negative large values
    c3 = (-5.07e30, -2.54e30)
    r3 = abs(c3)
    if (abs(r3 - r2) > 1e25) error stop "Test 3 failed"

    ! Asymmetric large values (|im| > |re|)
    c4 = (2.54e30, 5.07e30)
    r4 = abs(c4)
    if (abs(r4 - r2) > 1e25) error stop "Test 4 failed"

    ! Double precision large values
    z1 = (1.0d150, 2.0d150)
    d1 = abs(z1)
    if (d1 < 2.0d150 .or. d1 > 3.0d150) error stop "Test 5 failed: overflow in double complex abs()"

    ! Double precision with |im| > |re|
    z2 = (2.0d150, 1.0d150)
    d2 = abs(z2)
    if (abs(d2 - d1) > 1d145) error stop "Test 6 failed"

    print *, "All complex abs() overflow tests passed!"
end program
