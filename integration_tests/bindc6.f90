program bindc6
    use iso_c_binding, only: c_double
    implicit none
    real(c_double) :: arr(3)
    real(c_double) :: result

    interface
        function sum_arr(a, n) bind(c)
            import :: c_double
            real(c_double), intent(in) :: a(*)
            integer, value :: n
            real(c_double) :: sum_arr
        end function
    end interface

    arr = [1.0d0, 2.0d0, 3.0d0]
    result = sum_arr(arr, 3)
    if (abs(result - 6.0d0) > 1.0d-10) error stop "sum should be 6.0"
    print *, "PASS"
end program
