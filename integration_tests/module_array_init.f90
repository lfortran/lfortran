module mod_module_array_init
    implicit none
    integer, dimension(2) :: arr_integer_with_zero = 0
    integer, dimension(3) :: arr_integer_with_unary_minus = -1
    real, dimension(2) :: arr_real_with_zero = 0
    real, dimension(4) :: arr_real_with_unary_minus = -1
    complex, dimension(2) :: arr_complex_with_zero = (0.0, 0.0)
    logical, dimension(2) :: arr_logical_with_false = .false.
    ! TODO: the below commented out declarations don't work correctly
    ! in LLVM backend
    ! logical, dimension(2) :: arr_logical_with_true = .true.
    ! logical, dimension(2) :: arr_logical_with_true_false = [.true., .false.]
end module

program module_array_init
    use mod_module_array_init
    implicit none
    print *, arr_integer_with_zero
    if (any(arr_integer_with_zero /= 0)) error stop
    print *, arr_integer_with_unary_minus
    if (any(arr_integer_with_unary_minus /= -1)) error stop

    print*, arr_real_with_zero
    if (any(arr_real_with_zero /= 0.)) error stop
    print *, arr_real_with_unary_minus
    if (any(arr_real_with_unary_minus /= -1.)) error stop

    print *, arr_complex_with_zero
    if (any(arr_complex_with_zero /= (0., 0.))) error stop

    print *, arr_logical_with_false
    if (any(arr_logical_with_false .neqv. .false.)) error stop
    ! TODO: comment out this once the declarations above
    ! works
    ! print *, arr_logical_with_true
    ! if (any(arr_logical_with_true .neqv. .true.)) error stop
    ! print *, arr_logical_with_true_false
    ! if (arr_logical_with_false(1) .neqv. .true.) error stop
    ! if (arr_logical_with_false(2) .neqv. .false.) error stop
end program
