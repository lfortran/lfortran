program c_f_pointer_01
    use iso_c_binding, only: c_ptr, c_f_pointer
    type(c_ptr) :: queries
    integer(2), pointer :: x(:)
    integer :: shape(2, 2)
    call c_f_pointer(queries, x, shape)
end program
