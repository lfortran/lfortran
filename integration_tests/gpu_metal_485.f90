program gpu_metal_485
    ! Loop nests of two and three dimensions whose extents are not whole
    ! tiles of a two-dimensional launch: every iteration has to run once,
    ! and none outside the nest.
    implicit none
    integer, parameter :: n1 = 37, n2 = 13
    real :: a(0:n1 + 1, 0:n2 + 1), lap(n1, n2)
    integer :: b(3, 1000), c(0:4, -1:5, 2:9), d(70, 5), e(5, 4)
    integer :: f(300, 1), g(300, 2), h(40, 37, 1), p(3, 2, 500)
    integer :: i, j, k
    real :: expected

    do j = 0, n2 + 1
        do i = 0, n1 + 1
            a(i, j) = real(mod(i * 7 + j * 13, 17))
        end do
    end do
    lap = -1.0

    ! A stencil that reads the rows above and below.
    do concurrent (j = 1:n2, i = 1:n1)
        lap(i, j) = a(i - 1, j) + a(i + 1, j) + a(i, j - 1) + a(i, j + 1) &
            - 4.0 * a(i, j)
    end do
    do j = 1, n2
        do i = 1, n1
            expected = a(i - 1, j) + a(i + 1, j) + a(i, j - 1) &
                + a(i, j + 1) - 4.0 * a(i, j)
            if (abs(lap(i, j) - expected) > 1.0e-5) error stop 1
        end do
    end do

    ! A short contiguous dimension.
    b = -1
    do concurrent (j = 1:1000, i = 1:3)
        b(i, j) = i * 10000 + j
    end do
    do j = 1, 1000
        do i = 1, 3
            if (b(i, j) /= i * 10000 + j) error stop 2
        end do
    end do

    ! Three dimensions with lower bounds other than one, the loop naming
    ! the contiguous dimension first.
    c = -1
    do concurrent (i = 0:4, j = -1:5, k = 3:9)
        c(i, j, k) = i + 10 * j + 100 * k
    end do
    do k = 2, 9
        do j = -1, 5
            do i = 0, 4
                if (k == 2) then
                    if (c(i, j, k) /= -1) error stop 3
                else
                    if (c(i, j, k) /= i + 10 * j + 100 * k) error stop 4
                end if
            end do
        end do
    end do

    ! Part of each dimension only.
    d = 0
    do concurrent (j = 2:4, i = 3:68)
        d(i, j) = 1
    end do
    if (sum(d) /= 3 * 66) error stop 5
    if (any(d(1:2, :) /= 0) .or. any(d(69:70, :) /= 0)) error stop 6
    if (any(d(:, 1) /= 0) .or. any(d(:, 5) /= 0)) error stop 7

    ! An empty nest runs nothing.
    e = 7
    do concurrent (j = 1:0, i = 1:5)
        e(i, j + 1) = 0
    end do
    if (any(e /= 7)) error stop 8
    ! A second dimension of one or two.
    f = -1
    do concurrent (j = 1:1, i = 1:300)
        f(i, j) = i
    end do
    do i = 1, 300
        if (f(i, 1) /= i) error stop 9
    end do
    g = -1
    do concurrent (j = 1:2, i = 1:300)
        g(i, j) = i + 1000 * j
    end do
    do j = 1, 2
        do i = 1, 300
            if (g(i, j) /= i + 1000 * j) error stop 10
        end do
    end do

    ! A two-dimensional problem stored in three dimensions, the loop
    ! naming the dimension of one first.
    h = -1
    do concurrent (k = 1:1, j = 1:37, i = 1:40)
        h(i, j, k) = i + 100 * j
    end do
    do j = 1, 37
        do i = 1, 40
            if (h(i, j, 1) /= i + 100 * j) error stop 11
        end do
    end do

    ! Two short dimensions in front of a long one.
    p = -1
    do concurrent (i = 1:3, j = 1:2, k = 1:500)
        p(i, j, k) = i + 10 * j + 100 * k
    end do
    do k = 1, 500
        do j = 1, 2
            do i = 1, 3
                if (p(i, j, k) /= i + 10 * j + 100 * k) error stop 12
            end do
        end do
    end do
    print *, "ok"
end program
