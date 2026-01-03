program array_op_19
implicit none

real :: x(10, 10), sx(10, 10)
x = 2.0
sx = softmax(x)
print *, sx
if( any(abs(sx - 0.1) > 1e-6) ) error stop

contains

function softmax(x) result(y)
    real, intent(in) :: x(:, :)
    real :: y(size(x, 1), size(x, 2))
    integer :: i
    real :: s

    do i = 1, size(x, 2)
        y(:, i) = exp(x(:, i) - maxval(x(:, i)))
        y(:, i) = y(:, i) / sum(y(:, i))
    end do
end function

end program
