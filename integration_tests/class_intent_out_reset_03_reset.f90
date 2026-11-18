module class_intent_out_reset_03_reset_m
    use class_intent_out_reset_03_types_m, only: base
    implicit none
contains
    subroutine reset(x)
        class(base), intent(out) :: x
    end subroutine
end module
