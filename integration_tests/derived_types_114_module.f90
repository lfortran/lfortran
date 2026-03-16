! Test: intrinsic assignment of a derived type uses defined assignment
! for components that have assignment(=) defined.
module derived_types_114_module
    implicit none

    type :: ref_counter_t
        integer, pointer :: count_ => null()
    contains
        procedure :: assign_ref_counter
        generic :: assignment(=) => assign_ref_counter
        final :: finalize_ref_counter
    end type

    type :: wrapper_t
        type(ref_counter_t) :: counter
        integer :: val
    end type

contains

    subroutine assign_ref_counter(lhs, rhs)
        class(ref_counter_t), intent(inout) :: lhs
        class(ref_counter_t), intent(in) :: rhs
        lhs%count_ => rhs%count_
        if (associated(lhs%count_)) lhs%count_ = lhs%count_ + 1
    end subroutine

    subroutine finalize_ref_counter(self)
        type(ref_counter_t), intent(inout) :: self
        if (associated(self%count_)) then
            self%count_ = self%count_ - 1
            if (self%count_ == 0) then
                deallocate(self%count_)
            else
                self%count_ => null()
            end if
        end if
    end subroutine

end module
