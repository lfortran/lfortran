program fortran_01
    use iso_c_binding, only: c_ptr, c_loc
    implicit none
    integer, pointer :: x
    type(c_ptr) :: ptr
    ptr = c_loc(x)
end program