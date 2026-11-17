! Without a polymorphic entity, this file still lays out the vtables of
! `base` and `child`, to copy `holder`. Linked first, its vtables can be the
! ones that the reset in class_intent_out_reset_03_reset.f90 dispatches
! through.
module class_intent_out_reset_03_types_m
    implicit none
    type :: base
        integer :: root = -1
    end type
    type, extends(base) :: child
        integer :: step = 2
        integer, allocatable :: work(:)
    end type
    type :: holder
        type(child), allocatable :: elements(:)
    end type
end module

module class_intent_out_reset_03_copy_m
    use class_intent_out_reset_03_types_m, only: child, holder
    implicit none
contains
    subroutine copy_objects(x, y)
        type(child), intent(inout) :: x(:)
        type(child), intent(in) :: y(:)
        x = y
    end subroutine

    subroutine copy_holder(x, y)
        type(holder), intent(inout) :: x
        type(holder), intent(in) :: y
        x = y
    end subroutine
end module
