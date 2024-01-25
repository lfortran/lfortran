module integer_func_m

    public :: integer_zero, integer_one

contains 

    pure function integer_zero()
        integer :: integer_zero
        integer_zero = 0.
    end function

    pure function integer_one()
        integer :: integer_one
        integer_one = 1.
    end function

end module

program template_matrix_integer

    use matrix_m, only: matrix_tmpl
    use integer_func_m

    implicit none

    integer, parameter :: n = 2
    instantiate matrix_tmpl(integer, operator(+), integer_zero, operator(*), integer_one, n), only: &
            integer_matrix => matrix, &
            integer_plus_matrix => plus_matrix, &
            integer_times_matrix => times_matrix, &
            integer_matrix_subtraction_tmpl => matrix_subtraction_tmpl

end program