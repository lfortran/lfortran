program elemental_11
    implicit none

    real, dimension(3) :: array_param, expected_ans
    real :: scalar_param, epsilon = 1.0e-6
    real, dimension(3) :: result_2param
    real, dimension(3) :: result_3param

    array_param = [ 1.0, 2.0, 3.0 ]
    scalar_param = 1.0

    print *, "Testing 2-parameter elemental function:"

    result_2param = func2(array_param, array_param)
    expected_ans = [ 2.0, 4.0, 6.0 ]
    if ( size(result_2param) /= size(expected_ans) .or. .not. all(result_2param == expected_ans) ) then
        error stop "Array, Array does not match to expected array"
    end if

    result_2param = func2(array_param, scalar_param)
    expected_ans = [ 2.0, 3.0, 4.0 ]
    if ( size(result_2param) /= size(expected_ans) .or. .not. all(result_2param == expected_ans) ) then
        error stop "Array, Scalar does not match to expected array"
    end if

    result_2param = func2(scalar_param, array_param)
    expected_ans = [ 2.0, 3.0, 4.0 ]
    if ( size(result_2param) /= size(expected_ans) .or. .not. all(result_2param == expected_ans) ) then
        error stop "Scalar, Array does not match to expected array"
    end if

    result_2param = func2(scalar_param, scalar_param)
    expected_ans = [ 2.0, 0.0, 0.0 ]
    if (abs(result_2param(1) - expected_ans(1)) > epsilon) then
        error stop "Scalar, Scalar does not match 2.0"
    end if

    print *, "\nTesting 3-parameter elemental function:"

    result_3param = func3(array_param, array_param, array_param)
    expected_ans = [ 2.0, 6.0, 12.0 ]
    if ( size(result_3param) /= size(expected_ans) .or. .not. all(result_3param == expected_ans) ) then
        error stop "Array, Array, Array does not match to expected array"
    end if

    result_3param = func3(array_param, array_param, scalar_param)
    expected_ans = [ 2.0, 5.0, 10.0 ]
    if ( size(result_3param) /= size(expected_ans) .or. .not. all(result_3param == expected_ans) ) then
        error stop "Array, Array, Scalar does not match to expected array"
    end if

    result_3param = func3(array_param, scalar_param, array_param)
    expected_ans = [ 2.0, 4.0, 6.0 ]
    if ( size(result_3param) /= size(expected_ans) .or. .not. all(result_3param == expected_ans) ) then
        error stop "Array, Scalar, Array does not match to expected array"
    end if

    result_3param = func3(array_param, scalar_param, scalar_param)
    expected_ans = [ 2.0, 3.0, 4.0 ]
    if ( size(result_3param) /= size(expected_ans) .or. .not. all(result_3param == expected_ans) ) then
        error stop "Array, Scalar, Scalar does not match to expected array"
    end if

    result_3param = func3(scalar_param, array_param, array_param)
    expected_ans = [ 2.0, 4.0, 6.0 ]
    if ( size(result_3param) /= size(expected_ans) .or. .not. all(result_3param == expected_ans) ) then
        error stop "Scalar, Array, Array does not match to expected array"
    end if

    result_3param = func3(scalar_param, array_param, scalar_param)
    expected_ans = [ 2.0, 3.0, 4.0 ]
    if ( size(result_3param) /= size(expected_ans) .or. .not. all(result_3param == expected_ans) ) then
        error stop "Scalar, Array, Scalar does not match to expected array"
    end if

    result_3param = func3(scalar_param, scalar_param, array_param)
    expected_ans = [ 2.0, 3.0, 4.0 ]
    if ( size(result_3param) /= size(expected_ans) .or. .not. all(result_3param == expected_ans) ) then
        error stop "Scalar, Scalar, Array does not match to expected array"
    end if

    result_3param = func3(scalar_param, scalar_param, scalar_param)
    expected_ans = [ 2.0, 0.0, 0.0 ]
    if (abs(result_3param(1) - expected_ans(1)) > epsilon) then
        error stop "Scalar, Scalar, Scalar does not match 2.0"
    end if

    print *, "All tests passed successfully"

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

end program elemental_11
