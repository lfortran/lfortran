program deallocate_04
    ! deallocate of a pointer component whose target has an allocatable
    ! component must also free that component (no leak with --detect-leaks)
    implicit none
    type :: node
        integer :: val = 0
        integer, allocatable :: arr(:)
    end type
    type :: box
        type(node), pointer :: ptr => null()
    end type
    type(box) :: b
    allocate(b%ptr)
    b%ptr%val = 42
    allocate(b%ptr%arr(10))
    b%ptr%arr = 3
    if (b%ptr%val /= 42) error stop
    if (sum(b%ptr%arr) /= 30) error stop

    deallocate(b%ptr)
    if (associated(b%ptr)) error stop
    print *, "PASS"
end program
