program gpu_kernel_source_15
    ! A kernel, a block in it and a routine it calls declare only the
    ! scalars and locals they refer to: the kernel does not use the
    ! extent of `x` or `y` or the named constant `n`, but it does use the
    ! leading extent of `z`; the block does not use `unused`, and the
    ! function does not use `width` or `unused`.
    implicit none
    integer, parameter :: n = 8
    real, allocatable :: x(:), y(:), z(:,:)
    real :: a
    integer :: i
    allocate(x(n), y(n), z(n, 2))
    a = 2.0
    x = 1.0
    y = 3.0
    z = 0.0
    do concurrent (i = 1:n)
        block
            real :: t, unused
            t = a*x(i)
            y(i) = t + y(i)
        end block
        z(i, 2) = twice(y(i))
    end do
    if (any(abs(y - 5.0) > 1e-6)) error stop 1
    if (any(abs(z(:, 2) - 10.0) > 1e-6)) error stop 2
    if (any(z(:, 1) /= 0.0)) error stop 3
    print *, "ok"
contains
    pure real function twice(v)
        real, intent(in) :: v
        integer, parameter :: width = 2
        real :: unused
        twice = 2*v
    end function
end program
