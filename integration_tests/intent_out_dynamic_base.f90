module intent_out_dynamic_base
    implicit none
    type :: base
        integer :: inherited = 4
    end type
contains
    subroutine reset(x)
        class(base), intent(out) :: x
    end subroutine

    subroutine reset_any(x)
        class(*), intent(out) :: x
    end subroutine

    subroutine reset_array(x)
        class(base), intent(out), optional :: x(-1:, 0:)
    end subroutine

    subroutine forward(x)
        class(base), intent(inout), optional :: x(:, :)
        call reset_array(x)
    end subroutine
end module
