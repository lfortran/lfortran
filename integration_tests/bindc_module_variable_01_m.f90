module bindc_module_variable_01_m
    use iso_c_binding, only: c_int
    implicit none
    integer(c_int), bind(c, name="f2py_shared_count") :: shared_count = 5
contains
    subroutine bump()
        shared_count = shared_count + 1
    end subroutine
end module
