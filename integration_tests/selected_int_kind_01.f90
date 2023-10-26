subroutine test_selected_int_kind(R, expected_value)
    integer :: R, expected_value
    print *, "selected_int_kind(R) = ", selected_int_kind(R)
    if (selected_int_kind(R) /= expected_value) error stop
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
end  program
