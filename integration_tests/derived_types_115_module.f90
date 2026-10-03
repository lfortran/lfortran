! Test: defined assignment with non-struct RHS should not be called
! when doing intrinsic struct-to-struct copy.
module derived_types_115_module
    implicit none

    type :: inner_t
        logical :: test_passed = .false.
    contains
        procedure :: assign_logical
        generic :: assignment(=) => assign_logical
    end type

    type :: outer_t
        type(inner_t) :: component
        integer :: val = 0
    end type

contains

    subroutine assign_logical(lhs, rhs)
        class(inner_t), intent(out) :: lhs
        logical, intent(in) :: rhs
        lhs%test_passed = rhs
    end subroutine

end module
