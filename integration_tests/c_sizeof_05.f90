program c_sizeof_05
    use, intrinsic :: iso_c_binding
    implicit none

    integer(c_int) :: my_integer
    integer(c_size_t), parameter :: byte_size = c_sizeof(my_integer)

    real(c_double) :: my_double
    integer(c_size_t), parameter :: double_size = c_sizeof(my_double)

    print *, byte_size
    if (byte_size /= 4) error stop
    print *, double_size
    if (double_size /= 8) error stop

end program c_sizeof_05
