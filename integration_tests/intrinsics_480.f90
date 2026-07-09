program intrinsics_480
    ! Test co_reduce intrinsic subroutine (no-op in single-image mode).
    ! In single-image mode co_reduce leaves its first argument unchanged
    ! since there is only one image contributing to the reduction.
    implicit none
    logical :: b_and
    integer :: s_add
    integer :: stat_val
    character(len=32) :: err_msg

    b_and = .true.
    call co_reduce(b_and, and_op)
    if (.not. b_and) error stop

    b_and = .false.
    call co_reduce(b_and, and_op)
    if (b_and) error stop

    s_add = 5
    call co_reduce(s_add, add_op)
    if (s_add /= 5) error stop

    call co_reduce(s_add, add_op, result_image=1)
    if (s_add /= 5) error stop

    call co_reduce(s_add, add_op, 1, stat_val)
    if (s_add /= 5) error stop

    call co_reduce(s_add, add_op, 1, stat_val, err_msg)
    if (s_add /= 5) error stop

    print *, "co_reduce: all tests passed"
contains
    pure function and_op(lhs, rhs) result(res)
        logical, intent(in) :: lhs, rhs
        logical :: res
        res = lhs .and. rhs
    end function

    pure function add_op(lhs, rhs) result(res)
        integer, intent(in) :: lhs, rhs
        integer :: res
        res = lhs + rhs
    end function
end program intrinsics_480
