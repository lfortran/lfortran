program arrays_124
    implicit none
    real :: a(5), b(5), c(5)
    real :: x(3, 3), y(3, 3)
    integer :: i

    a = [(real(i), i = 1, 5)]
    b = 0.0
    c = 0.0

    b(:) = a(:) * 2.0
    if (abs(b(3) - 6.0) > 1e-6) error stop

    where (a(:) > 3.0)
        c(:) = a(:)
    elsewhere
        c(:) = -a(:)
    end where
    if (abs(c(1) + 1.0) > 1e-6) error stop
    if (abs(c(4) - 4.0) > 1e-6) error stop

    x = reshape([(real(i), i = 1, 9)], [3, 3])
    y = 0.0
    y(:,:) = compute(x(:,:))
    if (abs(y(1,1) - 2.0) > 1e-6) error stop
    if (abs(y(3,3) - 18.0) > 1e-6) error stop

contains
    function compute(inp) result(res)
        real, intent(in) :: inp(:,:)
        real :: res(size(inp,1), size(inp,2))
        res = inp * 2.0
    end function
end program
