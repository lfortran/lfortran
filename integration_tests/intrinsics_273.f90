program intrinsic_146
    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32

    integer :: i

    real(dp), parameter :: res_1(5) = dprod([5.2, 3.2, -4.1, 9.8, 5.6], [6.12, 39.2, -41.1, 32.1, 4.5])
    real(dp), parameter :: res_2(5) = dprod([5.2, 3.2, -4.1, 9.8, 5.6], 5.2)
    real(dp), parameter :: res_3(5) = dprod(5.2, [6.12, 39.2, -41.1, 32.1, 4.5])

    real(sp) :: x(5) = [5.2, 3.2, -4.1, 9.8, 5.6]
    real(sp) :: y(5) = [6.12, 39.2, -41.1, 32.1, 4.5]
    real(sp) :: z(5) = [6.12, 39.2, -41.1, 32.1, 4.5]

    real(sp) :: u = 5.2

    real(dp) :: run_x(5)
    real(dp) :: run_y(5)
    real(dp) :: run_z(5)

    real(dp) :: expected_1(5) = [3.18239982376098851e+01, 1.25440004310607947e+02, 1.68509989824295189e+02, &
            3.14579991168975539e+02, 2.51999995708465576e+01]
    real(dp) :: expected_2(5) = [2.70399990081787109e+01, 1.66400002479553244e+01, -2.13199995040893562e+01, &
            5.09600009918212891e+01, 2.91199995040893569e+01]
    real(dp) :: expected_3(5) = [3.18239994049072266e+01, 2.03840003967285156e+02, -2.13719992065429693e+02, &
            1.66919982910156250e+02, 2.33999996185302734e+01]

    do i = 1, 5
        print *, res_1(i)
        if (abs(res_1(i) - expected_1(i)) > 1e-5) error stop
    end do

    do i = 1, 5
        print *, res_2(i)
        if (abs(res_2(i) - expected_2(i)) > 1e-5) error stop
    end do

    do i = 1, 5
        print *, res_3(i)
        if (abs(res_3(i) - expected_3(i)) > 1e-5) error stop
    end do

    run_x = dprod(x, y)
    run_y = dprod(x, u)
    run_z = dprod(u, z)

    do i = 1, 5
        print *, run_x(i)
        if (abs(run_x(i) - expected_1(i)) > 1e-5) error stop
    end do

    do i = 1, 5
        print *, run_y(i)
        if (abs(run_y(i) - expected_2(i)) > 1e-5) error stop
    end do

    do i = 1, 5
        print *, run_z(i)
        if (abs(run_z(i) - expected_3(i)) > 1e-5) error stop
    end do

end program
