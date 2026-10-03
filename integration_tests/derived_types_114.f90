! Test: intrinsic assignment of a derived type uses defined assignment
! for components that have assignment(=) defined.
program derived_types_114
    use derived_types_114_module
    implicit none

    type(wrapper_t) :: a, b

    ! Set up a with a reference count of 1
    allocate(a%counter%count_, source=1)
    a%val = 42

    ! Intrinsic assignment of wrapper_t should use defined assignment
    ! for the counter component (incrementing the count)
    b = a

    ! Verify defined assignment was used: count should be 2
    if (.not. associated(b%counter%count_)) error stop
    if (b%counter%count_ /= 2) error stop

    ! Verify other members are copied
    if (b%val /= 42) error stop

    ! Both a and b share the same count pointer
    if (.not. associated(a%counter%count_, b%counter%count_)) error stop

    print *, "PASS"
end program
