program ilp64_logical_compare_01
    implicit none
    logical :: flag1, flag2, flag3, flag4, flag5, flag6

    flag1 = 5 >= 5
    if (.not. flag1) error stop
    flag2 = 5 > 3
    if (.not. flag2) error stop
    flag3 = 3 < 5
    if (.not. flag3) error stop
    flag4 = 3 <= 5
    if (.not. flag4) error stop
    flag5 = 5 == 5
    if (.not. flag5) error stop
    flag6 = 5 /= 3
    if (.not. flag6) error stop

    flag1 = 3 >= 5
    if (flag1) error stop
    flag2 = 3 > 5
    if (flag2) error stop
    flag3 = 5 < 3
    if (flag3) error stop
    flag4 = 5 <= 3
    if (flag4) error stop
    flag5 = 3 == 5
    if (flag5) error stop
    flag6 = 5 /= 5
    if (flag6) error stop

    print *, "All tests passed"
end program
