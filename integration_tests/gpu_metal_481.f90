program gpu_metal_481
    implicit none
    real :: z(-2:3, 2), bias(7:9), out(3, 2, 8)
    real :: z2(2, 3, 2), bias2(-1:0, 4:6), out2(2, 3, 2)
    integer :: i, k, j

    do i = 1, 2
        do k = -2, 3
            z(k, i) = real(k + 10*i)
        end do
    end do
    bias = [1., 3., 5.]
    do k = 1, 3
        do j = 1, 2
            bias2(j-2,k+3) = real(j+k)
            z2(j,k,1) = real(2*j+3*k)
            z2(j,k,2) = real(4*j+k)
        end do
    end do
    do concurrent (i = 1:2)
        block
            real :: t(-4:-2)
            t = plus(z(-2:2:2, i), bias)
            out(:, i, 1) = t
            t = plus(bias, z(3:-1:-2, i))
            out(:, i, 2) = t
            t = plus(real(int(z(-2:2:2, i), 4)), bias)
            out(:, i, 3) = t
            t = plus(z(-2:2:2, i), 0.5)
            out(:, i, 4) = t
            t = plus(0.25, bias)
            out(:, i, 5) = t
            out(:, i, 6) = plus(z(-2:2:2, i), broadcast(i))
            t = plus(bias, whole_sum(bias))
            out(:, i, 7) = t
            t = real(small(z(-2:2:2, i), int(bias, 4)))
            out(:, i, 8) = t
        end block
        block
            real :: t(-4:-3, 8:10)
            t = plus(z2(:, :, i), bias2)
            out2(:, :, i) = t
        end block
    end do
    do i = 1, 2
        do k = 1, 3
            if (abs(out(k,i,1) - (real(2*k-4+10*i) + bias(k+6))) > 1.e-6) error stop 1
            if (abs(out(k,i,2) - (bias(k+6) + real(5-2*k+10*i))) > 1.e-6) error stop 2
            if (abs(out(k,i,3) - out(k,i,1)) > 1.e-6) error stop 3
            if (abs(out(k,i,4) - (real(2*k-4+10*i) + 0.5)) > 1.e-6) error stop 4
            if (abs(out(k,i,5) - (0.25 + bias(k+6))) > 1.e-6) error stop 5
            if (abs(out(k,i,6) - (real(2*k-4+10*i) + 0.5*i)) > 1.e-6) error stop 6
            if (abs(out(k,i,7) - (bias(k+6) + 9.)) > 1.e-6) error stop 7
            if (abs(out(k,i,8) - out(k,i,1)) > 1.e-6) error stop 8
            do j = 1, 2
                if (abs(out2(j,k,i) - (z2(j,k,i) + bias2(j-2,k+3))) > 1.e-6) error stop 9
            end do
        end do
    end do
    print *, "PASS"
contains
    pure elemental real function plus(x, y) result(r)
        real, intent(in) :: x, y
        r = x + y
    end function
    pure elemental integer(4) function small(x, y) result(r)
        real, intent(in) :: x
        integer(4), intent(in) :: y
        r = int(x, 4) + y
    end function
    pure real function broadcast(i) result(r)
        integer, intent(in) :: i
        r = 0.5*i
    end function
    pure real function whole_sum(x) result(r)
        real, intent(in) :: x(:)
        r = sum(x)
    end function
end program
