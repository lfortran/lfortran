program c_sizeof_07
    use iso_c_binding, only: c_int32_t, c_double, c_size_t, c_sizeof
    implicit none
    integer(c_int32_t) :: scalar, vector(2), matrix(3, 4), empty(0, 3)
    real(c_double) :: real_matrix(2, 3)

    call check_integer(scalar, c_sizeof(scalar))
    call check_integer(vector, 2 * c_sizeof(scalar))
    call check_integer(matrix, 12 * c_sizeof(scalar))
    call check_integer(empty, 0_c_size_t)
    call check_real(real_matrix, 6 * c_sizeof(0.0_c_double))
    call check_shape(vector, 2 * c_sizeof(scalar))
contains
    subroutine check_integer(x, expected)
        integer(c_int32_t), intent(in) :: x(..)
        integer(c_size_t), intent(in) :: expected
        if (c_sizeof(x) /= expected) error stop 1
    end subroutine

    subroutine check_real(x, expected)
        real(c_double), intent(in) :: x(..)
        integer(c_size_t), intent(in) :: expected
        if (c_sizeof(x) /= expected) error stop 2
    end subroutine

    subroutine check_shape(x, expected)
        integer(c_int32_t), intent(in) :: x(:)
        integer(c_size_t), intent(in) :: expected
        if (c_sizeof(x) /= expected) error stop 3
    end subroutine
end program
