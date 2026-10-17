program gpu_metal_480
    implicit none
    integer, parameter :: count = 65
    real :: result(count), expected, x
    integer :: i, j, limit, limits(count)

    do i = 1, count
        limits(i) = modulo(i, 37) - 2
    end do
    limits(count) = -2147483647 - 1
    do concurrent (i = 1:count)
        block
            integer :: j, n
            real :: value, x
            n = limits(i)
            value = 0
            do j = 1, n
                x = real(j)
                value = value + sqrt(x*x + 1.0) + x*0.25
                n = n - 1
            end do
            do j = 0, modulo(i, 23) - 1
                value = value + real(j)*0.5
            end do
            do j = 10, 2, -2
                value = value + real(j)*0.125
            end do
            result(i) = value
        end block
    end do

    do i = 1, count
        expected = 0
        limit = limits(i)
        do j = 1, limit
            x = real(j)
            expected = expected + sqrt(x*x + 1.0) + x*0.25
        end do
        do j = 0, modulo(i, 23) - 1
            expected = expected + real(j)*0.5
        end do
        do j = 10, 2, -2
            expected = expected + real(j)*0.125
        end do
        if (abs(result(i) - expected) > 1e-5*max(1.0, abs(expected))) error stop
    end do
    print *, "ok"
end program
