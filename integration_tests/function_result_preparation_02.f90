program function_result_preparation_02
    implicit none
    integer :: calls, dimension_calls, copies_calls, n, d
    real :: a(2, 3), b(3, 2), r(12)

    a = 1.0
    b = 2.0
    calls = 0
    dimension_calls = 0
    copies_calls = 0
    n = 2
    d = 2
    r = flatten3(spread(a + next_value(), next_dimension(), next_copies()))
    if (calls /= 1) error stop 1
    if (any(abs(r - 3.0) > 1e-6)) error stop 2
    if (dimension_calls /= 1 .or. copies_calls /= 1) error stop 5
    r(:4) = flatten2(matmul(2.0*a, b + next_value()))
    if (calls /= 2) error stop 3
    if (any(abs(r(:4) - 30.0) > 1e-6)) error stop 4
contains
    real function next_value()
        calls = calls + 1
        next_value = real(calls + 1)
    end function

    integer function next_dimension()
        dimension_calls = dimension_calls + 1
        next_dimension = d
    end function

    integer function next_copies()
        copies_calls = copies_calls + 1
        next_copies = n
    end function

    function flatten3(x) result(y)
        real, intent(in) :: x(:,:,:)
        real :: y(size(x))
        y = reshape(x, [size(x)])
    end function

    function flatten2(x) result(y)
        real, intent(in) :: x(:,:)
        real :: y(size(x))
        y = reshape(x, [size(x)])
    end function
end program
