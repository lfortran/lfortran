program pointer_22
    ! allocate a host-associated pointer array inside a contained subroutine (#4506)
    implicit none
    integer, dimension(:), pointer :: int_ptr

    nullify(int_ptr)
    call sub
    if (.not. associated(int_ptr)) error stop
    if (size(int_ptr) /= 2) error stop
    if (int_ptr(1) /= 1 .or. int_ptr(2) /= 2) error stop
    print *, int_ptr
    deallocate(int_ptr)

contains

    subroutine sub
        allocate(int_ptr(2))
        int_ptr = [1, 2]
    end subroutine sub

end program
