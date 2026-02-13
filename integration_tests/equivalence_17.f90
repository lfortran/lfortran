program equivalence_17
  implicit none
  call test_complex_real_array()
  call test_real_array_complex()
  call test_scalar_scalar()
  call test_array_array()

contains

  subroutine test_complex_real_array()
    complex :: c1
    real :: r1(2)
    equivalence (c1, r1)

    r1 = [-42.0, -43.0]
    c1 = cmplx(0.0, 0.0)
    if (abs(r1(1)) > 1e-6) error stop "r1(1) should be 0.0 after c1 assignment"
    if (abs(r1(2)) > 1e-6) error stop "r1(2) should be 0.0 after c1 assignment"

    r1 = [3.0, 4.0]
    if (abs(real(c1) - 3.0) > 1e-6) error stop "real(c1) should be 3.0"
    if (abs(aimag(c1) - 4.0) > 1e-6) error stop "aimag(c1) should be 4.0"
  end subroutine

  subroutine test_real_array_complex()
    real :: r2(2)
    complex :: c2
    equivalence (r2, c2)

    c2 = cmplx(5.0, 6.0)
    if (abs(r2(1) - 5.0) > 1e-6) error stop "r2(1) should be 5.0"
    if (abs(r2(2) - 6.0) > 1e-6) error stop "r2(2) should be 6.0"

    r2 = [7.0, 8.0]
    if (abs(real(c2) - 7.0) > 1e-6) error stop "real(c2) should be 7.0"
    if (abs(aimag(c2) - 8.0) > 1e-6) error stop "aimag(c2) should be 8.0"
  end subroutine

  subroutine test_scalar_scalar()
    integer :: i1
    integer :: i2
    equivalence (i1, i2)

    i1 = 42
    if (i2 /= 42) error stop "i2 should be 42 after i1 assignment"

    i2 = 99
    if (i1 /= 99) error stop "i1 should be 99 after i2 assignment"
  end subroutine

  subroutine test_array_array()
    integer :: a1(4)
    integer :: a2(4)
    equivalence (a1, a2)

    a1 = [10, 20, 30, 40]
    if (a2(1) /= 10) error stop "a2(1) should be 10"
    if (a2(2) /= 20) error stop "a2(2) should be 20"
    if (a2(3) /= 30) error stop "a2(3) should be 30"
    if (a2(4) /= 40) error stop "a2(4) should be 40"

    a2 = [50, 60, 70, 80]
    if (a1(1) /= 50) error stop "a1(1) should be 50"
    if (a1(4) /= 80) error stop "a1(4) should be 80"
  end subroutine

end program equivalence_17
