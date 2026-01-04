program test_allocatable_struct_with_allocatable_components
    implicit none
    type :: container_t
        integer, allocatable :: data(:)
    end type
    type(container_t), allocatable :: arg

    ! Initial allocation
    allocate(arg)
    allocate(arg%data(5))
    arg%data = [1, 2, 3, 4, 5]

    ! Call with intent(out) - must deallocate arg on entry
    call process(arg)

    ! Verify re-allocation worked
    if (.not. allocated(arg)) error stop "arg should be allocated after call"
    if (size(arg%data) /= 3) error stop "wrong size"
    if (any(arg%data /= [10, 20, 30])) error stop "wrong data"

    print *, "PASS"

contains
    subroutine process(x)
        type(container_t), allocatable, intent(out) :: x

        ! Intent(out) must deallocate on entry
        if (allocated(x)) error stop "intent(out) must deallocate on entry"

        ! Allocate fresh
        allocate(x)
        allocate(x%data(3))
        x%data = [10, 20, 30]
    end subroutine
end program
