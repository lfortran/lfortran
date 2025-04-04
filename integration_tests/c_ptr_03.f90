program c_ptr_03
    use iso_c_binding, only: c_ptr, c_loc, c_int, c_f_pointer
    implicit none
    integer(c_int), target :: x, y
    type(c_ptr), dimension(2) :: c_requests
    integer(c_int), pointer :: px, py

    x = 10
    y = 20

    c_requests(1) = c_loc(x)
    c_requests(2) = c_loc(x)
    call c_f_pointer(c_requests(1), px)
    call c_f_pointer(c_requests(2), py)

    print *, "px: ", px
    if (px /= x) error stop

    print *, "py: ", py
    if (py /= y) error stop
end program c_ptr_03
