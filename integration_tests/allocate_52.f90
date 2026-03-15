module allocate_52_mod
    implicit none
    type :: node
        integer :: val = 0
    end type
    type :: box
        type(node), pointer :: ptr => null()
    end type
contains
    subroutine alloc_ptr(p)
        type(node), pointer :: p
        allocate(p)
        p%val = 42
    end subroutine
end module

program allocate_52
    use allocate_52_mod
    implicit none
    type(box) :: b

    call alloc_ptr(b%ptr)
    if (.not. associated(b%ptr)) error stop
    if (b%ptr%val /= 42) error stop

    deallocate(b%ptr)
    print *, "PASS"
end program
