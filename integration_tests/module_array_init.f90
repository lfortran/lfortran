module mod_module_array_init
    implicit none
    integer, dimension(2) :: arr_integer_with_zero = 0
    real, dimension(2) :: arr_real_with_zero = 0
    complex, dimension(2) :: arr_complex_with_zero = (0.0, 0.0)
end module

program module_array_init
    use mod_module_array_init
    implicit none
    if (any(arr_integer_with_zero /= [0, 0])) error stop
    if (any(arr_real_with_zero /= [0., 0.])) error stop
    if (any(arr_complex_with_zero /= [(0., 0.), (0., 0.)])) error stop

    if (kind(arr_integer_with_zero(1)) /= 4) error stop
    if (kind(arr_real_with_zero(1)) /= 4) error stop
    if (kind(arr_complex_with_zero(1)) /= 4) error stop
end program
