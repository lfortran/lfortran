function f(x) result(r)
    real, intent(in) :: x(:)
    real :: r(size(x))
    r = x
end function

program array_op_12
    implicit none
    interface
        function f(x) result(r)
            real, intent(in) :: x(:)
            real :: r(size(x))
        end function
    end interface
    integer :: i
    real :: a(2)
    a = f([(real(i), i=1,2)]) + 1.0
    if (abs(a(1) - 2.0) > 1e-6) error stop
    if (abs(a(2) - 3.0) > 1e-6) error stop
    print *, "ok"
end program
