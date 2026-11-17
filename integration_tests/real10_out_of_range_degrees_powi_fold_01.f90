program real10_out_of_range_degrees_powi_fold_01
    ! out_of_range, sind/cosd/tand and x**n of real(10) constants are folded
    ! exactly, or like the run time, not through double
    implicit none
    ! out_of_range: 2**63 - 0.5 truncates to huge(1_8) and rounds to 2**63;
    ! -2**63 - 0.75 is -2**63 - 1 in real(10)
    real(10), parameter :: o(6) = [2.0_10**63 - 0.5_10, -2.0_10**63 - 0.75_10, &
        2.0_10**63, -2.0_10**63, 2.0_10**31 - 0.25_10, -2.0_10**31 - 0.75_10]
    logical, parameter :: t8(6) = out_of_range(o, 1_8)
    logical, parameter :: r8(6) = out_of_range(o, 1_8, .true.)
    logical, parameter :: t4(6) = out_of_range(o, 1)
    logical, parameter :: r4(6) = out_of_range(o, 1, .true.)
    logical, parameter :: f4(2) = out_of_range([1.0e38_10, 4.0e38_10], 1.0_4)
    logical, parameter :: f10(2) = out_of_range([1.0e38_10, -4.0e4000_10], 1.0_10)
    logical, parameter :: s8 = out_of_range(o(3), 1_8)

    ! sind/cosd/tand of a large angle: 3**30 degrees is 9 degrees modulo 360,
    ! 30 - 3**30 degrees is -339 degrees
    real(10), parameter :: d(2) = [3.0_10**30, -3.0_10**30 + 30.0_10]
    real(10), parameter :: sd(2) = sind(d), cd(2) = cosd(d), td(2) = tand(d)
    real(10), parameter :: s9 = 0.156434465040230869010_10
    real(10), parameter :: c9 = 0.987688340595137726190_10
    real(10), parameter :: t9 = 0.158384440324536293839_10
    real(10), parameter :: c21 = 0.933580426497201748990_10
    real(10), parameter :: s21 = 0.358367949545300273484_10
    real(10), parameter :: tol = 4 * epsilon(1.0_10)

    ! x**n: real(10) power by an integer, computed like the run time
    real(10), parameter :: x(3) = [3.0_10**30, -7.75_10, 1.0_10 + epsilon(1.0_10)]
    real(10), parameter :: pw(3) = x**(-2), pw3(2) = x(2:3)**3
    real(10) :: xv(3), w(3)

    if (any(t8 .neqv. [.false., .true., .true., .false., .false., .false.])) error stop
    if (any(r8 .neqv. [.true., .true., .true., .false., .false., .false.])) error stop
    if (any(t4 .neqv. [.true., .true., .true., .true., .false., .false.])) error stop
    if (.not. all(r4)) error stop
    if (any(f4 .neqv. [.false., .true.])) error stop
    if (any(f10)) error stop
    if (.not. s8) error stop

    if (abs(sd(1) - s9) > tol .or. abs(cd(1) - c9) > tol) error stop
    if (abs(td(1) - t9) > tol) error stop
    if (abs(sd(2) - s21) > tol .or. abs(cd(2) - c21) > tol) error stop

    xv = x
    w = xv**(-2)
    if (any(pw /= w)) error stop
    w(2:3) = xv(2:3)**3
    if (any(pw3 /= w(2:3))) error stop
    print *, t8, sd, pw
end program
