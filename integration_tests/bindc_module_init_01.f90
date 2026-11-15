module bindc_module_init_01_m
    use iso_c_binding, only: c_int
    implicit none
    integer(c_int), bind(C, name="visible_c_global") :: bound = 7_c_int
end module

program bindc_module_init_01
    use iso_c_binding, only: c_int
    use bindc_module_init_01_m, only: bound
    implicit none
    if (bound /= 7_c_int) error stop 1
    bound = 9_c_int
    if (bound /= 9_c_int) error stop 2
end program
