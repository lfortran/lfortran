! Test elemental intrinsics on multi-dimensional parameter arrays.
! Verifies that aimag, abs, and real preserve the rank of the input.
program elemental_19
    implicit none
    integer, parameter :: sp = kind(1.0)

    ! 2D complex parameter array
    complex(sp), parameter :: z(2, 3) = reshape([ &
        (1., 2.), (3., 4.), (5., 6.), (7., 8.), (9., 10.), (11., 12.) &
    ], [2, 3])

    ! 2D integer parameter array
    integer, parameter :: a(2, 3) = reshape([1, -2, 3, -4, 5, -6], [2, 3])

    real(sp) :: yr(2, 3), yi(2, 3)
    integer :: ai(2, 3)

    ! aimag on 2D parameter must preserve rank 2
    yi = aimag(z)
    if (any(shape(yi) /= [2, 3])) error stop
    if (abs(yi(1,1) - 2.) > 1e-6) error stop
    if (abs(yi(2,1) - 4.) > 1e-6) error stop
    if (abs(yi(1,2) - 6.) > 1e-6) error stop
    if (abs(yi(2,3) - 12.) > 1e-6) error stop

    ! real() on 2D parameter must preserve rank 2
    yr = real(z)
    if (any(shape(yr) /= [2, 3])) error stop
    if (abs(yr(1,1) - 1.) > 1e-6) error stop
    if (abs(yr(2,1) - 3.) > 1e-6) error stop
    if (abs(yr(1,3) - 9.) > 1e-6) error stop

    ! abs() on 2D integer parameter must preserve rank 2
    ai = abs(a)
    if (any(shape(ai) /= [2, 3])) error stop
    if (ai(1,1) /= 1) error stop
    if (ai(2,1) /= 2) error stop
    if (ai(1,2) /= 3) error stop
    if (ai(2,2) /= 4) error stop
    if (ai(2,3) /= 6) error stop

    print *, "PASS"
end program
