module bindc_module_variable_01_m
    use iso_c_binding, only: c_int, c_char
    implicit none
    integer(c_int), bind(c, name="f2py_shared_count") :: shared_count = 5
    character(kind=c_char), bind(c) :: shared_char = 'a'
    character(kind=c_char), bind(c) :: shared_chars(3) = ['a', 'b', 'c']
contains
    subroutine bump()
        shared_count = shared_count + 1
        shared_char = 'b'
        shared_chars(3) = 'd'
    end subroutine
end module
