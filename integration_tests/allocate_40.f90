program allocate_40
    ! Test allocated() and deallocate for class(*) allocatable scalars
    implicit none
    class(*), allocatable :: obj

    if (allocated(obj)) error stop

    obj = 42
    if (.not. allocated(obj)) error stop

    deallocate(obj)
    if (allocated(obj)) error stop

    obj = 3.14
    if (.not. allocated(obj)) error stop

    deallocate(obj)
    if (allocated(obj)) error stop

    print *, "PASS"
end program allocate_40
