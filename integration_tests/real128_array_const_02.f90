program real128_array_const_02
    ! Array intrinsics over real(16) and complex(16) parameter arrays
    implicit none
    real(16), parameter :: m(4) = [1, 2, 3, 4]
    complex(16), parameter :: a(2) = [(1.5_16, 2.25_16), (2.0_16, 1.0_16)]
    real(16) :: r(4), ra(2)
    complex(16) :: c(2), pc(2)
    real(16), parameter :: tol = 1.0e-25_16

    r = pack(m, .true.)
    if (any(abs(r - m) > tol)) error stop
    r = eoshift(m, 1)
    if (abs(r(1) - 2) > tol .or. abs(r(3) - 4) > tol .or. abs(r(4)) > tol) error stop
    r = cshift(m, 1)
    if (abs(r(1) - 2) > tol .or. abs(r(4) - 1) > tol) error stop
    r = merge(m, 0*m, .true.)
    if (any(abs(r - m) > tol)) error stop

    c = pack(a, .true.)
    if (abs(c(1) - a(1)) > tol .or. abs(c(2) - a(2)) > tol) error stop
    c = merge(a, 2*a, .true.)
    if (abs(c(2) - a(2)) > tol) error stop
    c = eoshift(a, 1, (0.0_16, 0.0_16))
    if (abs(c(1) - a(2)) > tol .or. abs(c(2)) > tol) error stop
    c = sqrt(a)
    pc = c*c
    if (abs(pc(1) - a(1)) > 1.0e-30_16 .or. abs(pc(2) - a(2)) > 1.0e-30_16) error stop
    c = exp(a)
    if (abs(c(2) - exp((2.0_16, 1.0_16))) > tol) error stop
    c = sin(a)
    if (abs(c(1) - sin((1.5_16, 2.25_16))) > tol) error stop
    ra = abs(a)
    if (abs(ra(2) - sqrt(5.0_16)) > tol) error stop
    print *, "ok"
end program
