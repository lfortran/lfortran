! Test: allocatable struct member passed to non-allocatable struct intent(out)
! The allocatable actual should NOT be deallocated when dummy is non-allocatable
! This is a regression test for incorrect ImplicitDeallocate generation
module intent_out_struct_member_no_dealloc_m
    implicit none

    type :: inner_t
        integer :: val
    end type inner_t

    type :: wrapper_t
        type(inner_t), allocatable :: inner
    end type wrapper_t

contains
    ! Non-allocatable struct intent(out) - should NOT deallocate actual
    subroutine set_inner(x)
        type(inner_t), intent(out) :: x
        x%val = 99
    end subroutine set_inner
end module intent_out_struct_member_no_dealloc_m

program intent_out_struct_member_no_dealloc
    use intent_out_struct_member_no_dealloc_m
    implicit none
    type(wrapper_t) :: w
    logical :: was_allocated_before, is_allocated_after

    allocate(w%inner)
    w%inner%val = 1
    was_allocated_before = allocated(w%inner)

    ! w%inner is allocatable struct member
    ! But set_inner takes non-allocatable struct intent(out)
    ! This should NOT deallocate w%inner before the call
    call set_inner(w%inner)

    is_allocated_after = allocated(w%inner)

    ! Verify allocation status was preserved
    if (.not. was_allocated_before) error stop 1
    if (.not. is_allocated_after) then
        print *, "BUG: w%inner was incorrectly deallocated!"
        error stop 2
    end if
    if (w%inner%val /= 99) error stop 3

    print *, "Test passed"
end program intent_out_struct_member_no_dealloc
