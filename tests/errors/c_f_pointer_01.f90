program c_f_pointer_01
    use iso_c_binding, only: c_ptr, c_f_pointer
    type(c_ptr) :: queries
    integer, pointer :: x
    call c_f_pointer(queries, x, [2])
end program
