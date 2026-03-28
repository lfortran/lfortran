program logical_kind_06
    implicit none
    logical(1) :: x
    logical(1) :: arr(3)
    logical :: res

    ! Test scalar .eqv. with kind mismatch
    x = .true.
    res = x .eqv. .true.
    if (res .neqv. .true.) error stop

    x = .false.
    res = x .eqv. .true.
    if (res .neqv. .false.) error stop

    ! Test passing .eqv. result to subroutine expecting default logical
    x = .true.
    call check_val(x .eqv. .true., .true.)
    call check_val(x .eqv. .false., .false.)
    call check_val(x .neqv. .true., .false.)
    call check_val(x .neqv. .false., .true.)

    ! Test with all() intrinsic and .eqv.
    arr = .true.
    call check_val(all(arr) .eqv. .true., .true.)

    arr(2) = .false.
    call check_val(all(arr) .eqv. .true., .false.)

    ! Test with generic interface
    call check_generic(all(arr) .eqv. .false., "test")

    print *, "All tests passed"
contains
    subroutine check_val(actual, expected)
        logical, intent(in) :: actual, expected
        if (actual .neqv. expected) error stop
    end subroutine

    subroutine check_generic(expression, msg)
        logical, intent(in) :: expression
        character(*), intent(in) :: msg
        if (.not. expression) error stop msg
    end subroutine
end program
