program real10_real16_min_max_01
    ! min and max of real(10) and real(16) constant arrays (mixed kinds are an
    ! extension): the real(10) values are widened to real(16) exactly
    implicit none
    real(10), parameter :: a(3) = [1.5_10, -2.5_10, 1.0_10 + 2.0_10**(-63)]
    real(16), parameter :: b(3) = [0.25_16, 0.5_16, 1.0_16 + 2.0_16**(-100)]
    real(16), parameter :: emin(3) = [0.25_16, -2.5_16, 1.0_16 + 2.0_16**(-100)]
    real(16), parameter :: emax(3) = [1.5_16, 0.5_16, 1.0_16 + 2.0_16**(-63)]
    real(16), parameter :: pmin(3) = min(a, b), pmax(3) = max(b, a)
    real(16) :: r(3)

    if (any(pmin /= emin) .or. any(pmax /= emax)) error stop
    r = min(a, b)
    if (any(r /= emin)) error stop
    r = max(b, a)
    if (any(r /= emax)) error stop
    r = min(b, a)
    if (any(r /= emin)) error stop
    r = max(a, b)
    if (any(r /= emax)) error stop
    print *, r
end program
