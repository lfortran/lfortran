program separate_compilation_49
    use mod_separate_compilation_49
    implicit none

    type(mytype) :: t
    real, allocatable :: p(:), g(:), all_p(:)
    integer :: i

    ! Setup
    allocate(t%weights(2, 3))
    allocate(t%dw(2, 3))
    t%weights = 1.0
    t%dw = 2.0

    ! Case 1: 1D pointer => null() in function
    p = get_params_1d(t)
    if (size(p) /= 6) error stop "FAIL case1: wrong size"
    do i = 1, size(p)
        if (abs(p(i) - 1.0) > 1e-6) error stop "FAIL case1: wrong value"
    end do
    print *, "PASS case 1: 1D pointer => null() in function"

    ! Case 2: second 1D pointer => null() in different function
    g = get_gradients_1d(t)
    if (size(g) /= 6) error stop "FAIL case2: wrong size"
    do i = 1, size(g)
        if (abs(g(i) - 2.0) > 1e-6) error stop "FAIL case2: wrong value"
    end do
    print *, "PASS case 2: second 1D pointer => null() in different function"

    ! Case 3: 2D pointer => null() in subroutine
    t%weights = 0.0
    p = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    call set_params_2d(t, p)
    if (abs(t%weights(1,1) - 1.0) > 1e-6) error stop "FAIL case3: wrong value"
    if (abs(t%weights(2,3) - 6.0) > 1e-6) error stop "FAIL case3: wrong value"
    print *, "PASS case 3: 2D pointer => null() in subroutine"

    ! Case 4: multiple pointers in same function
    all_p = get_all(t)
    if (size(all_p) /= 12) error stop "FAIL case4: wrong size"
    print *, "PASS case 4: multiple pointers => null() in same function"

    print *, "ALL CASES PASSED"

end program separate_compilation_49
