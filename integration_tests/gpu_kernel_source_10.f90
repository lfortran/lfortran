program gpu_kernel_source_10
    implicit none
    real :: x(4), y(4), z(4)
    integer :: i
    x = [1., 2., 3., 4.]
    do concurrent (i = 1:4)
        y(i) = x(i)*2.0 + 1.0
        z(i) = multiply_add(x(i), 2.0)
    end do
    if (any(y /= [3., 5., 7., 9.])) error stop 1
    if (any(z /= [3., 5., 7., 9.])) error stop 2
    print *, "ok"
contains
    pure real function multiply_add(a, b)
        real, intent(in) :: a, b
        multiply_add = a*b + 1.0
    end function
end program
