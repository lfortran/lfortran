program array_reshape_14

call test_reshape_with_argument()

contains

subroutine test_nd_to_1d(a)
    real(8), intent(in) :: a(:, :)
    integer :: i, j, k, l
    real(8) :: eps
    real(8) :: b(256)
    integer :: newshape(1)
    real(8) :: c(16, 16, 16)
    real(8) :: d(4096)
    integer :: newshape1(1)
    eps = 1e-12

    newshape(1) = 256
    b = reshape(a, newshape)
    do k = 1, 256
        i = (k - 1)/16
        j = (k - 1) - i*16
        if (abs(b(k) - i - j - 0.5) > eps) error stop
    end do

    do i = 1, 16
        do j = 1, 16
            do k = 1, 16
                c(i, j, k) = (i - 1) + (j - 1) + (k - 1) + 0.5
            end do
        end do
    end do

    newshape1(1) = 4096
    d = reshape(c, newshape1)
    do l = 1, 4096
        i = (l - 1)/256
        j = ((l - 1) - i*256)/16
        k = ((l - 1) - i*256 - j*16)
        if (abs(d(l) - i - j - k - 0.5) > eps) error stop
    end do
end subroutine

subroutine test_1d_to_nd(d)
    real(8) :: d(:)
    integer :: i, j, k, l
    real(8) :: eps
    real(8) :: b(256)
    real(8) :: a(16, 16)
    integer :: newshape(2)
    real(8) :: c(16, 16, 16)
    integer :: newshape1(3)
    eps = 1e-12

    do k = 1, 256
        i = (k - 1)/16
        j = (k - 1) - i*16
        b(k) = i + j + 0.5
    end do

    newshape(1) = 16
    newshape(2) = 16
    a = reshape(b, newshape)
    do i = 1, 16
        do j = 1, 16
            if (abs(a(i, j) - (i - 1) - (j - 1) - 0.5) > eps) error stop
        end do
    end do

    newshape1(1) = 16
    newshape1(2) = 16
    newshape1(3) = 16
    c = reshape(d, newshape1)
    do i = 1, 16
        do j = 1, 16
            do k = 1, 16
                if (abs(c(i, j, k) - (i - 1) - (j - 1) - (k - 1) - 0.5) > eps) error stop
            end do
        end do
    end do
end subroutine

subroutine test_reshape_with_argument()
    integer :: i, j, k, l
    real(8) :: a(16, 16)
    real(8) :: d(4096)

    do i = 1, 16
        do j = 1, 16
            a(i, j) = (i - 1) + (j - 1) + 0.5
        end do
    end do

    call test_nd_to_1d(a)

    do l = 1, 4096
        i = (l - 1)/256
        j = ((l - 1) - i*256)/16
        k = ((l - 1) - i*256 - j*16)
        d(l) = i + j + k + 0.5
    end do

    call test_1d_to_nd(d)
end subroutine

end program
