! The base type and a procedure with a polymorphic intent(out) dummy of it,
! compiled before any extension exists (intent_out_dynamic_type_01).
module intent_out_dynamic_type_01_base
    implicit none
    integer :: base_finals = 0
    type :: Base
        integer :: n = 7
    contains
        final :: base_final
    end type
contains
    subroutine base_final(self)
        type(Base), intent(inout) :: self
        base_finals = base_finals + 1
    end subroutine

    subroutine reset(x)
        class(Base), intent(out) :: x
        if (x%n /= 7) error stop 90
    end subroutine
end module
