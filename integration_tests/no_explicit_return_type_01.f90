program no_explicit_return_type
    real, dimension(3) :: arr, squared_arr
    real :: val
    arr = [1.0, 2.0, 3.0]

    print *, "f_elemental_real(1.0): ", f_elemental_real(1.0)
    if (abs(f_elemental_real(1.0) - 43.0) > 1e-12) error stop

    print *, "f_pure_real(1.0): ", f_pure_real(1.0)
    if (abs(f_pure_real(1.0) - 24.0) > 1e-12) error stop

    squared_arr = square(arr)
    print *, "squared_arr: ", squared_arr
    if (any(squared_arr /= [1.0, 4.0, 9.0])) error stop

    val = impure_log(10.0)
    print *, "val: ", val
    if ( abs(val - 2.0) > 1e-12 ) error stop

    contains

    ! elemental function without explicit type
    elemental function f_elemental_real(x)
        real, intent(in) :: x
        f_elemental_real = x + 42.0
    end function

    ! pure function without explicit type
    pure function f_pure_real(x)
        real, intent(in) :: x
        f_pure_real = x + 23.0
    end function

    ! pure elemental function without explicit type
    pure elemental real function square(x)
        real, intent(in) :: x
        square = x * x
    end function square

    ! impure function without explicit type
    impure function impure_log(a)
        real, intent(in) :: a
        impure_log = log(a)
    end function impure_log
end program no_explicit_return_type
