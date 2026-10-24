program gpu_metal_485
    ! Loop nests of two and three dimensions. On CUDA the nests marked
    ! "tiled" run in tiles of 32 by 8 iterations whose last tiles are
    ! partial; the nests marked "flat" have a dimension so short that whole
    ! tiles would pad them by more than a quarter, so they run one thread
    ! per iteration. Every iteration has to run once, and none outside the
    ! nest.
    implicit none
    integer, parameter :: n1 = 62, n2 = 62
    real :: a(0:n1 + 1, 0:n2 + 1), lap(n1, n2)
    integer :: b(3, 1000), c(-3:56, 0:29, 2:6), d(70, 40), e(5, 4)
    integer :: f(300, 1), g(300, 2), h(40, 37, 1), p(3, 2, 500)
    integer :: m(60, 30, 4), o(60, 4, 30), s(60, 4, 30), t(30, 60)
    integer :: u(64, 40)
    integer :: i, j, k, nz
    real :: expected

    do j = 0, n2 + 1
        do i = 0, n1 + 1
            a(i, j) = real(mod(i * 7 + j * 13, 17))
        end do
    end do
    lap = -1.0

    ! Tiled: a stencil that reads the rows above and below, partial tiles
    ! in both dimensions (62 = 32 + 30 = 7 * 8 + 6).
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

    ! Flat: a short contiguous dimension.
    b = -1
    do concurrent (j = 1:1000, i = 1:3)
        b(i, j) = i * 10000 + j
    end do
    do j = 1, 1000
        do i = 1, 3
            if (b(i, j) /= i * 10000 + j) error stop 2
        end do
    end do

    ! Tiled: three dimensions with lower bounds other than one and partial
    ! tiles, the loop naming the contiguous dimension first and covering
    ! part of the third dimension only.
    c = -1
    do concurrent (i = -3:56, j = 0:29, k = 3:6)
        c(i, j, k) = i + 100 * j + 10000 * k
    end do
    do k = 2, 6
        do j = 0, 29
            do i = -3, 56
                if (k == 2) then
                    if (c(i, j, k) /= -1) error stop 3
                else
                    if (c(i, j, k) /= i + 100 * j + 10000 * k) error stop 4
                end if
            end do
        end do
    end do

    ! Tiled: part of each dimension only, partial tiles in both.
    d = 0
    do concurrent (j = 3:33, i = 3:64)
        d(i, j) = 1
    end do
    if (sum(d) /= 31 * 62) error stop 5
    if (any(d(1:2, :) /= 0) .or. any(d(65:70, :) /= 0)) error stop 6
    if (any(d(:, 1:2) /= 0) .or. any(d(:, 34:40) /= 0)) error stop 7

    ! An empty nest runs nothing.
    e = 7
    do concurrent (j = 1:0, i = 1:5)
        e(i, j + 1) = 0
    end do
    if (any(e /= 7)) error stop 8

    ! Flat: a second dimension of one or two.
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

    ! Flat: a two-dimensional problem stored in three dimensions, the loop
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

    ! Flat: two short dimensions in front of a long one.
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

    ! Tiled: the contiguous dimension in the middle of the loop order, the
    ! tile's height along j, last in loop order.
    m = -1
    do concurrent (k = 1:4, i = 1:60, j = 1:30)
        m(i, j, k) = i + 100 * j + 10000 * k
    end do
    do k = 1, 4
        do j = 1, 30
            do i = 1, 60
                if (m(i, j, k) /= i + 100 * j + 10000 * k) error stop 13
            end do
        end do
    end do

    ! Tiled: stores that disagree on the second subscript, so the tile's
    ! height runs along k, the next dimension in loop order (along j, of
    ! extent 4, whole tiles would pad the nest too much).
    o = -1
    s = -1
    do concurrent (k = 1:30, j = 1:4, i = 1:60)
        o(i, j, k) = i + 100 * j + 10000 * k
        s(i, 5 - j, k) = 1
    end do
    do k = 1, 30
        do j = 1, 4
            do i = 1, 60
                if (o(i, j, k) /= i + 100 * j + 10000 * k) error stop 14
                if (s(i, j, k) /= 1) error stop 15
            end do
        end do
    end do

    ! Tiled: a transposed store, contiguous in j, which the loop names
    ! last.
    t = -1
    do concurrent (i = 1:60, j = 1:30)
        t(j, i) = j + 100 * i
    end do
    do i = 1, 60
        do j = 1, 30
            if (t(j, i) /= j + 100 * i) error stop 17
        end do
    end do

    ! An empty dimension outside the tile runs nothing.
    nz = 0
    u = 3
    do concurrent (k = 1:nz, j = 1:40, i = 1:64)
        u(i, j + k) = 0
    end do
    if (any(u /= 3)) error stop 18
    print *, "ok"
end program
