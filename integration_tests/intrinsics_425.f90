program intrinsics_425
    ! Test co_sum with optional arguments (no-op in single-image mode)
    implicit none
    integer :: a, stat_val
    real :: b

    a = 10
    call co_sum(a, 1)
    if (a /= 10) error stop

    b = 3.14
    call co_sum(b, 1)
    if (abs(b - 3.14) > 1e-6) error stop

    a = 5
    stat_val = 0
    call co_sum(a, result_image=1, stat=stat_val)
    if (a /= 5) error stop

    print *, "co_sum with optional args: all tests passed"
end program intrinsics_425
