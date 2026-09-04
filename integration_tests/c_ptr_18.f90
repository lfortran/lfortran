module c_ptr_18_mod
    use iso_c_binding
    implicit none
contains
    subroutine take_cptr_ref(ptr, success)
        type(c_ptr), intent(in) :: ptr
        logical, intent(out) :: success
        if (.not. c_associated(ptr)) then
            success = .true.
        else
            success = .false.
        end if
    end subroutine take_cptr_ref
end module c_ptr_18_mod

program c_ptr_18
    use iso_c_binding
    use c_ptr_18_mod
    implicit none
    type(c_ptr) :: my_ptr
    logical :: ok
    my_ptr = c_null_ptr
    call take_cptr_ref(my_ptr, ok)
    if (.not. ok) error stop "c_ptr_18 test failed"
    print *, "c_ptr_18 test passed"
end program c_ptr_18
