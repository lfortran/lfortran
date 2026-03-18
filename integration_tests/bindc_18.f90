module bindc_18_mod
    use iso_c_binding, only: c_int
    implicit none

    interface
        ! C function: receives assumed-shape a(:) as CFI descriptor, returns sum
        integer(c_int) function c_sum_array(a) bind(C, name="c_sum_array")
            import :: c_int
            integer(c_int), intent(in) :: a(:)
        end function

        ! C function: receives assumed-shape a(:) as CFI descriptor, doubles in-place
        subroutine c_double_array(a) bind(C, name="c_double_array")
            import :: c_int
            integer(c_int), intent(inout) :: a(:)
        end subroutine
    end interface
end module

program bindc_18
    use bindc_18_mod
    use iso_c_binding, only: c_int
    implicit none

    integer(c_int) :: arr(4)
    integer(c_int) :: total

    arr = [10, 20, 30, 40]

    ! Test 1: sum via C function receiving CFI descriptor
    total = c_sum_array(arr)
    print *, "c_sum_array =", total
    if (total /= 100) error stop "FAIL: expected sum = 100"

    ! Test 2: double in-place via C function receiving CFI descriptor
    call c_double_array(arr)
    print *, "after c_double_array:", arr
    if (arr(1) /= 20) error stop "FAIL: arr(1)"
    if (arr(2) /= 40) error stop "FAIL: arr(2)"
    if (arr(3) /= 60) error stop "FAIL: arr(3)"
    if (arr(4) /= 80) error stop "FAIL: arr(4)"

    ! Test 3: sum again after doubling
    total = c_sum_array(arr)
    print *, "c_sum_array after double =", total
    if (total /= 200) error stop "FAIL: expected sum = 200"

    print *, "All tests passed."
end program
