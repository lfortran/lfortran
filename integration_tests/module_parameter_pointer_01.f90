module module_parameter_pointer_01_m
    implicit none
    integer, parameter :: max_value = 100
end module

subroutine module_parameter_pointer_01_init(cb)
    use module_parameter_pointer_01_m, only: max_value
    external cb
    call cb(max_value)
end subroutine

subroutine module_parameter_pointer_01_touch()
    integer, allocatable :: a(:)
    allocate(a(4000))
    a = 7
end subroutine
