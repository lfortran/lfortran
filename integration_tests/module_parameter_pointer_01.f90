module module_parameter_pointer_01_m
    implicit none
    integer, parameter :: max_value = 100
    real(8), parameter :: scale_val = 2.5d0
    integer, parameter :: arr_param(3) = [10, 20, 30]
end module

subroutine module_parameter_pointer_01_init(cb, cb_real, cb_arr)
    use module_parameter_pointer_01_m, only: max_value, scale_val, arr_param
    external cb, cb_real, cb_arr
    call cb(max_value)
    call cb_real(scale_val)
    call cb_arr(arr_param)
end subroutine

subroutine module_parameter_pointer_01_touch()
    integer, allocatable :: a(:)
    allocate(a(4000))
    a = 7
end subroutine
