module iso_c_binding_04_mod
    use iso_c_binding, only: c_ptr, c_f_pointer, c_loc, c_int
    implicit none
contains
    subroutine set_data(p, data)
        type(c_ptr), intent(in) :: p
        type(c_ptr), intent(in) :: data
        type(c_ptr), pointer :: ptr_data
        call c_f_pointer(p, ptr_data)
        ptr_data = data
    end subroutine
end module

program iso_c_binding_04
    use iso_c_binding_04_mod
    use iso_c_binding, only: c_ptr, c_loc, c_int, c_f_pointer
    implicit none
    type(c_ptr), target :: storage
    integer(c_int), target :: val
    integer(c_int), pointer :: iptr
    val = 42
    storage = c_loc(val)
    call set_data(c_loc(storage), storage)
    call c_f_pointer(storage, iptr)
    if (iptr /= 42) error stop
    print *, "ok"
end program
