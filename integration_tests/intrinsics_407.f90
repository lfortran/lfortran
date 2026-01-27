! Test complex abs() with extreme values (overflow and underflow protection)
! This tests the numerically stable algorithm for complex abs
program intrinsics_407
    implicit none
    complex :: c1, c2, c3, c4, c5, c6
    complex(8) :: z1, z2, z3, z4
    real :: r1, r2, r3, r4, r5, r6
    real(8) :: d1, d2, d3, d4

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

    ! ===== UNDERFLOW TESTS =====
    ! Small values that would underflow naively: x^2 + y^2 -> 0
    ! but the correct result is representable

    ! Single precision underflow test
    ! 1e-30^2 = 1e-60 underflows, but sqrt(2)*1e-30 ~ 1.41e-30 is representable
    c5 = (1.0e-30, 1.0e-30)
    r5 = abs(c5)
    ! Expected: sqrt(2) * 1e-30 ~ 1.414e-30
    if (r5 < 1.0e-30 .or. r5 > 2.0e-30) error stop "Test 7 failed: underflow in single complex abs()"

    ! Single precision underflow with asymmetric values
    c6 = (1.0e-30, 2.0e-30)
    r6 = abs(c6)
    ! Expected: sqrt(1 + 4) * 1e-30 ~ 2.236e-30
    if (r6 < 2.0e-30 .or. r6 > 3.0e-30) error stop "Test 8 failed: underflow in single complex abs()"

    ! Double precision underflow test
    ! 1d-200^2 = 1d-400 underflows, but sqrt(2)*1d-200 ~ 1.414d-200 is representable
    z3 = (1.0d-200, 1.0d-200)
    d3 = abs(z3)
    ! Expected: sqrt(2) * 1d-200 ~ 1.414d-200
    if (d3 < 1.0d-200 .or. d3 > 2.0d-200) error stop "Test 9 failed: underflow in double complex abs()"

    ! Double precision underflow with asymmetric values
    z4 = (1.0d-200, 2.0d-200)
    d4 = abs(z4)
    ! Expected: sqrt(1 + 4) * 1d-200 ~ 2.236d-200
    if (d4 < 2.0d-200 .or. d4 > 3.0d-200) error stop "Test 10 failed: underflow in double complex abs()"

    print *, "All complex abs() overflow and underflow tests passed!"
end program
