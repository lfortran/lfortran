module intent_out_dynamic_empty_base
    implicit none
    type :: empty
    end type
contains
    subroutine reset(x)
        class(empty), intent(out) :: x
    end subroutine
end module
