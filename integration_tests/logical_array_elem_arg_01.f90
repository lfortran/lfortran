program logical_array_elem_arg_01
    implicit none

    logical :: a(3)

    a = [.true., .false., .true.]

    call flip(a(2))
    if (a(2)) error stop

    call set_true(a(2))
    if (.not. a(2)) error stop

    print *, "ok"

contains

    subroutine flip(x)
        logical, intent(inout) :: x
        x = .not. x
    end subroutine flip

    subroutine set_true(x)
        logical, intent(out) :: x
        x = .true.
    end subroutine set_true

end program logical_array_elem_arg_01
