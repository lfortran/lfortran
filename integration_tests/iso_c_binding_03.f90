program iso_c_binding_03
    use iso_c_binding, only: c_ptr, c_f_pointer, c_loc, c_int, c_null_ptr
    implicit none
    type(c_ptr), pointer :: fptr
    type(c_ptr), target :: tgt
    integer(c_int), target :: val
    integer(c_int), pointer :: iptr

    val = 42
    tgt = c_loc(val)
    fptr => tgt
    call c_f_pointer(tgt, iptr)
    if (iptr /= 42) error stop
    print *, "ok"
end program
