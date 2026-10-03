module use_06_module_with_a_long_name_to_make_long_use_statements_ab
    implicit none
    integer :: a_module_variable_with_a_long_name_to_make_long_use_statements = 5
contains
    integer function a_function_with_a_long_name_to_make_the_use_statements_long(x)
        integer, intent(in) :: x
        a_function_with_a_long_name_to_make_the_use_statements_long = x + 1
    end function a_function_with_a_long_name_to_make_the_use_statements_long
end module use_06_module_with_a_long_name_to_make_long_use_statements_ab

program use_06
    ! Imports of long names do not fit on one line.
    use use_06_module_with_a_long_name_to_make_long_use_statements_ab, only: &
        a_function_with_a_long_name_to_make_the_use_statements_long
    use use_06_module_with_a_long_name_to_make_long_use_statements_ab, only: &
        a_local_name_that_is_also_long_to_make_long_use_statements_abcd => &
        a_module_variable_with_a_long_name_to_make_long_use_statements
    implicit none
    if (a_function_with_a_long_name_to_make_the_use_statements_long(1) /= 2) error stop
    if (a_local_name_that_is_also_long_to_make_long_use_statements_abcd /= 5) error stop
    print *, "ok"
end program use_06
