program test_single_ptr_value
    use iso_c_binding
    implicit none

    integer, target :: x
    integer, pointer :: fptr
    type(c_ptr), dimension(1) :: ptr_array

    x = 7

    ptr_array = [c_null_ptr]

    ptr_array(1) = c_loc(x)

    call c_f_pointer(ptr_array(1), fptr)

    print *, fptr

    if (fptr /= 7) error stop

end program test_single_ptr_value
