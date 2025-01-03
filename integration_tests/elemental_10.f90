program elemental_10
    implicit none

    real, dimension(1) :: array_param
    real :: scalar_param, expected_ans, epsilon = 1.0e-6
    real, dimension(1) :: result_2param
    real, dimension(1) :: result_3param

    array_param = [1.0]
    scalar_param = 1.0

    expected_ans = 2.0

    print *, "Testing 2-parameter elemental function:"

    result_2param = func2(array_param, array_param)
    if (any(abs(result_2param - 2.0) > epsilon)) then
        error stop "Array, Array does not match 1.0"
    end if

    result_2param = func2(array_param, scalar_param)
    if (any(abs(result_2param - expected_ans) > epsilon)) then
        error stop "Array, Scalar does not match 1.0"
    end if

    result_2param = func2(scalar_param, array_param)
    if (any(abs(result_2param - expected_ans) > epsilon)) then
        error stop "Scalar, Array does not match 1.0"
    end if

    result_2param = func2(scalar_param, scalar_param)
    if (abs(result_2param(1) - expected_ans) > epsilon) then
        error stop "Scalar, Scalar does not match 1.0"
    end if

    print *, "\nTesting 3-parameter elemental function:"

    result_3param = func3(array_param, array_param, array_param)
    if (any(abs(result_3param - expected_ans) > epsilon)) then
        error stop "Array, Array, Array does not match 1.0"
    end if

    result_3param = func3(array_param, array_param, scalar_param)
    if (any(abs(result_3param - expected_ans) > epsilon)) then
        error stop "Array, Array, Scalar does not match 1.0"
    end if

    result_3param = func3(array_param, scalar_param, array_param)
    if (any(abs(result_3param - expected_ans) > epsilon)) then
        error stop "Array, Scalar, Array does not match 1.0"
    end if

    result_3param = func3(array_param, scalar_param, scalar_param)
    if (any(abs(result_3param - expected_ans) > epsilon)) then
        error stop "Array, Scalar, Scalar does not match 1.0"
    end if

    result_3param = func3(scalar_param, array_param, array_param)
    if (any(abs(result_3param - expected_ans) > epsilon)) then
        error stop "Scalar, Array, Array does not match 1.0"
    end if

    result_3param = func3(scalar_param, array_param, scalar_param)
    if (any(abs(result_3param - expected_ans) > epsilon)) then
        error stop "Scalar, Array, Scalar does not match 1.0"
    end if

    result_3param = func3(scalar_param, scalar_param, array_param)
    if (any(abs(result_3param - expected_ans) > epsilon)) then
        error stop "Scalar, Scalar, Array does not match 1.0"
    end if

    result_3param = func3(scalar_param, scalar_param, scalar_param)
    if (abs(result_3param(1) - expected_ans) > epsilon) then
        error stop "Scalar, Scalar, Scalar does not match 1.0"
    end if

contains

    elemental function func2(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function func2

    elemental function func3(x, y, z) result(res)
        real, intent(in) :: x, y, z
        real :: res
        res = x * y + z
    end function func3

end program elemental_10
