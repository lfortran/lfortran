program array_op_13
    implicit none
    integer, parameter :: dp = kind(1.0d0)
    real(dp) :: c(2,2), r(2)
    c = 0.0_dp
    r = f(c, 1) - [1.0, 2.0]
    print *, r(1), r(2)
    if (abs(r(1) - (-1.0_dp)) > 1.0e-12_dp) error stop
    if (abs(r(2) - (-2.0_dp)) > 1.0e-12_dp) error stop
contains
    function f(x, dim) result(res)
        real(dp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        res = 0.0_dp
    end function
end program
