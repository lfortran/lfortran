module c_associated_pointer_01_m
    use iso_c_binding
    implicit none
    type :: ctx_t
        integer :: kind = 0
    end type ctx_t
contains
    function make(kind, handle_arg) bind(C, name="make") result(code)
        integer(c_int32_t), value :: kind
        type(c_ptr), value :: handle_arg
        type(c_ptr), pointer :: handle
        integer(c_int32_t) :: code
        type(ctx_t), pointer :: ctx
        code = 1
        if (.not. c_associated(handle_arg)) return
        call c_f_pointer(handle_arg, handle)
        code = 2
        if (c_associated(handle)) return
        allocate(ctx)
        ctx%kind = kind
        handle = c_loc(ctx)
        code = 0
    end function make
end module c_associated_pointer_01_m

program c_associated_pointer_01
    ! c_associated of a c_ptr with the POINTER attribute tests the value of
    ! its target, not the association of the pointer.
    use c_associated_pointer_01_m
    implicit none
    type(c_ptr), target :: h
    integer(c_int32_t) :: code
    h = c_null_ptr
    code = make(0_c_int32_t, c_loc(h))
    print *, code, c_associated(h)
    if (code /= 0 .or. .not. c_associated(h)) error stop 1
end program c_associated_pointer_01
