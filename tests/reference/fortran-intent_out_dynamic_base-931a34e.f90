module intent_out_dynamic_base
implicit none
type :: base
    integer(4) :: inherited = 4
end type base

contains

subroutine forward(x)
    class(base), dimension(:, :), intent(inout), optional :: x
    call reset_array(x)
end subroutine forward

subroutine lcompilers_intent_out_reset_base(object)
    type(base), intent(inout) :: object
end subroutine lcompilers_intent_out_reset_base

subroutine reset(x)
    class(base), intent(out) :: x
end subroutine reset

subroutine reset_any(x)
    class(*), intent(out) :: x
end subroutine reset_any

subroutine reset_array(x)
    class(base), dimension(-1:, 0:), intent(out), optional :: x
end subroutine reset_array

end module intent_out_dynamic_base

