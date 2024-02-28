function temp(x, dim) result(res)
    real, intent(in) :: x(:)
    integer, intent(in) :: dim
    real :: res

    res = sum(x, 1)
end function

program intrinsics_164
    real :: a(4)
    interface
        function temp(x, dim) result(res)
            real, intent(in) :: x(:)
            integer, intent(in) :: dim
            real :: res
        end function
    end interface

    a = [1.0, 2.0, 3.0, 4.0]
    print *, temp(a, 1)
    if (abs(temp(a, 1) - sum(a, 1)) > 1e-8) error stop
end program
