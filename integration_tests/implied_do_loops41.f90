program implied_do_loops41
    implicit none

    integer :: i
    integer, parameter :: dims(2) = [2, 2]
    integer, parameter :: values_mul(2) = [(dims*i, i=1,1)]
    integer, parameter :: values_add(2) = [(dims+i, i=2,2)]
    integer, parameter :: values_sub(2) = [(dims-i, i=1,1)]
    integer, parameter :: values_div(2) = [(dims/i, i=2,2)]
    
    integer, parameter :: exp_mul(2) = [2, 2]
    integer, parameter :: exp_add(2) = [4, 4]
    integer, parameter :: exp_sub(2) = [1, 1]
    integer, parameter :: exp_div(2) = [1, 1]

    if (any(values_mul /= exp_mul)) error stop "Mul failed"
    if (any(values_add /= exp_add)) error stop "Add failed"
    if (any(values_sub /= exp_sub)) error stop "Sub failed"
    if (any(values_div /= exp_div)) error stop "Div failed"

    print *, values_mul
end program implied_do_loops41
