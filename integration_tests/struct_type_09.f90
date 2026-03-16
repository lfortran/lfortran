module struct_type_09_mod
    use iso_c_binding, only: c_ptr, c_null_ptr
    implicit none

    type :: boxed_ptr
        type(c_ptr) :: p = c_null_ptr
    end type boxed_ptr

    type(boxed_ptr), save :: x
end module struct_type_09_mod

program struct_type_09
    use iso_c_binding, only: c_associated
    use struct_type_09_mod, only: x
    implicit none

    write(*, '(L1)') c_associated(x%p)
end program struct_type_09
