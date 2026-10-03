module struct_type_11_mod
    use, intrinsic :: iso_c_binding, only: c_funptr, c_null_funptr
    implicit none

    type, bind(c) :: callbacks
        type(c_funptr) :: cb = c_null_funptr
    end type callbacks
end module struct_type_11_mod

program struct_type_11
    use struct_type_11_mod, only: callbacks
    implicit none
    type(callbacks) :: x
    print *, "ok"
end program struct_type_11
