program c_ptr_09
    use iso_c_binding, only: c_ptr, c_null_ptr, c_associated
    implicit none
    type(c_ptr) :: p

    ! Test c_associated with c_null_ptr literal directly
    if (c_associated(c_null_ptr)) error stop

    ! Test c_associated with a variable holding c_null_ptr
    p = c_null_ptr
    if (c_associated(p)) error stop

end program c_ptr_09
