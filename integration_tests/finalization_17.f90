module scalar_intent_out_finalization
    implicit none
    integer :: scalars = 0, arrays = 0
    type item
        integer :: value = 0
    contains
        final :: finish_scalar
        final :: finish_array
    end type
contains
    subroutine finish_scalar(x)
        type(item), intent(inout) :: x
        if (x%value /= 7) error stop 1
        scalars = scalars + 1
    end subroutine
    subroutine finish_array(x)
        type(item), intent(inout) :: x(:)
        if (size(x) /= 2) error stop 2
        if (any(x%value /= 9)) error stop 3
        arrays = arrays + 1
    end subroutine
    subroutine reset(x)
        type(item), intent(out) :: x
        x%value = 8
    end subroutine
end module

program test_scalar_intent_out_finalization
    use scalar_intent_out_finalization
    implicit none
    type(item) :: x
    type(item), allocatable :: a(:)
    x%value = 7
    call reset(x)
    if (scalars /= 1 .or. arrays /= 0) error stop 4
    if (x%value /= 8) error stop 5
    allocate(a(2))
    a%value = 9
    deallocate(a)
    if (scalars /= 1 .or. arrays /= 1) error stop 6
    x%value = 7
end program
