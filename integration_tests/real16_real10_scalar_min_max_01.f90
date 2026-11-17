program real16_real10_scalar_min_max_01
    ! min and max of a real(16) constant array and a real(10) literal
    ! constant (mixed kinds are an extension): the real(10) value is widened
    ! to real(16) exactly. 1.00000000000000000011_10 is 1 + 2**-63.
    implicit none
    real(16), parameter :: b(3) = [0.25_16, 0.5_16, 1.0_16 + 2.0_16**(-100)]
    real(16), parameter :: ea = 1.0_16 + 2.0_16**(-63)
    real(16), parameter :: emax(3) = [ea, ea, ea]
    real(16), parameter :: emin(3) = [0.25_16, 0.5_16, 1.0_16 + 2.0_16**(-100)]
    real(16), parameter :: pmax(3) = max(b, 1.00000000000000000011_10)
    real(16), parameter :: pmin(3) = min(1.00000000000000000011_10, b)
    real(16) :: r(3)

    if (any(pmax /= emax) .or. any(pmin /= emin)) error stop
    r = max(b, 1.00000000000000000011_10)
    if (any(r /= emax)) error stop
    r = min(1.00000000000000000011_10, b)
    if (any(r /= emin)) error stop
    r = max(1.00000000000000000011_10, b)
    if (any(r /= emax)) error stop
    print *, r
end program
