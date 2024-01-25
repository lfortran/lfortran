module real_func_m

    public :: real_zero, real_one

contains 

    pure function real_zero()
        real :: real_zero
        real_zero = 0.
    end function

    pure function real_one()
        real :: real_one
        real_one = 1.
    end function

end module

program template_matrix_real

    use matrix_m, only: matrix_tmpl
    use real_func_m

    implicit none

    integer, parameter :: n = 2
    instantiate matrix_tmpl(real, operator(+), real_zero, operator(*), real_one, n), only: &
            real_matrix => matrix, &
            real_plus_matrix => plus_matrix, &
            real_times_matrix => times_matrix, &
            real_matrix_subtraction_tmpl => matrix_subtraction_tmpl

end program