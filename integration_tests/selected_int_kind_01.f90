subroutine test_selected_int_kind(R, expected_value)
    integer :: R, expected_value
    print *, "selected_int_kind(R) = ", selected_int_kind(R)
    if (selected_int_kind(R) /= expected_value) error stop
end subroutine

subroutine test_compile_time_selected_int_kind()
    print *, selected_int_kind(1)
    if (selected_int_kind(1) /= 1) error stop
    print *, selected_int_kind(2)
    if (selected_int_kind(2) /= 1) error stop
    print *, selected_int_kind(3)
    if (selected_int_kind(3) /= 2) error stop
    print *, selected_int_kind(4)
    if (selected_int_kind(4) /= 2) error stop
    print *, selected_int_kind(5)
    if (selected_int_kind(5) /= 4) error stop
    print *, selected_int_kind(6)
    if (selected_int_kind(6) /= 4) error stop
    print *, selected_int_kind(7)
    if (selected_int_kind(7) /= 4) error stop
    print *, selected_int_kind(8)
    if (selected_int_kind(8) /= 4) error stop
    print *, selected_int_kind(9)
    if (selected_int_kind(9) /= 4) error stop
    print *, selected_int_kind(10)
    if (selected_int_kind(10) /= 8) error stop
    print *, selected_int_kind(11)
    if (selected_int_kind(11) /= 8) error stop
end subroutine

program selected_int_kind_01
    integer :: x
    x = 1
    do while (x < 3)
        call test_selected_int_kind(x, 1)
        x = x + 1
    end do

    do while (x < 5)
        call test_selected_int_kind(x, 2)
        x = x + 1
    end do

    do while (x < 10)
        call test_selected_int_kind(x, 4)
        x = x + 1
    end do

    call test_selected_int_kind(x, 8)
    call test_compile_time_selected_int_kind()
end  program
