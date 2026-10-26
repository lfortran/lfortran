program realloc_lhs_fn_result_05
    implicit none

    real, allocatable :: x(:)

    ! The array result comes from an internal (contained) function, but the
    ! left hand side is still unallocated, so the runtime error must say so
    ! and point at the option that fixes it.
    x = make_array(3)

    if (size(x) /= 3) error stop

contains

    function make_array(n) result(y)
        integer, intent(in) :: n
        real :: y(n)
        y = 1.0
    end function make_array

end program realloc_lhs_fn_result_05
