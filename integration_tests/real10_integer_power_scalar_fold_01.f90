program real10_integer_power_scalar_fold_01
    ! A real(10) scalar constant raised to an integer power is folded like
    ! the run time computes it (by repeated multiplication), not with powl
    implicit none
    real(10), parameter :: x1 = 3.0_10**30, x2 = -7.75_10
    real(10), parameter :: x3 = 1.0_10 + epsilon(1.0_10)
    real(10), parameter :: p1 = x1**(-2), p2 = x2**(-2), p3 = 3.0_10**(-20)
    real(10), parameter :: p4 = x3**3, p5 = x2**3
    real(10) :: x

    x = x1
    if (p1 /= x**(-2)) error stop "p1"
    x = x2
    if (p2 /= x**(-2) .or. p5 /= x**3) error stop "p2"
    x = 3.0_10
    if (p3 /= x**(-20)) error stop "p3"
    x = x3
    if (p4 /= x**3) error stop "p4"
    print *, p1, p2, p3
end program
