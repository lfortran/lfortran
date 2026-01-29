! Test: complex literal kind with -fdefault-integer-8
! Complex literals with integer components should produce default complex kind (4),
! not complex(8), even when integers are promoted to 64-bit by -fdefault-integer-8.
program ilp64_complex_literal_01
    implicit none
    complex :: c1, c2
    complex :: arr(2)
    complex(8) :: c8

    ! Test 1: Simple complex literal assignment with integers
    c1 = (-1, 0)
    if (kind(c1) /= 4) error stop "c1 should have kind 4"
    if (abs(real(c1) - (-1.0)) > 1e-6) error stop "c1 real part wrong"
    if (abs(aimag(c1)) > 1e-6) error stop "c1 imag part wrong"

    ! Test 2: Complex literal in DATA statement
    data arr /(-1,0),(1,0)/
    if (kind(arr(1)) /= 4) error stop "arr should have kind 4"
    if (abs(real(arr(1)) - (-1.0)) > 1e-6) error stop "arr(1) real part wrong"
    if (abs(real(arr(2)) - 1.0) > 1e-6) error stop "arr(2) real part wrong"

    ! Test 3: Mixed integer/real should produce kind from real
    c2 = (-1, 0.0d0)
    if (kind((-1, 0.0d0)) /= 8) error stop "mixed int/real(8) should give kind 8"

    ! Test 4: Real(8) components should produce complex(8)
    c8 = (-1.0d0, 0.0d0)
    if (kind(c8) /= 8) error stop "c8 should have kind 8"

    print *, "PASS: all complex literal kind tests"
end program
