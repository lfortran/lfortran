module gpu_metal_511_m
    ! One-sided differences at the ends of a dimension and centred ones
    ! inside, as pure functions of the whole array.
    implicit none
contains
    pure real function diff_x(a, n, i, j) result(d)
        real, intent(in) :: a(:,:)
        integer, intent(in) :: n, i, j
        integer :: last
        last = n
        if (i == 1) then
            d = a(2, j) - a(1, j)
        else if (i == 2) then
            d = 0.5 * (a(3, j) - a(1, j))
        else if (i + 1 < last) then
            d = (a(i - 2, j) - 8 * a(i - 1, j) + 8 * a(i + 1, j) &
                - a(i + 2, j)) / 12
        else if (i == last - 1) then
            d = 0.5 * (a(last, j) - a(last - 2, j))
        else
            d = a(last, j) - a(last - 1, j)
        end if
    end function

    pure real function diff_y(a, m, i, j) result(d)
        real, intent(in) :: a(:,:)
        integer, intent(in) :: m, i, j
        if (j == 1) then
            d = a(i, 2) - a(i, 1)
        else if (j == m) then
            d = a(i, m) - a(i, m - 1)
        else
            d = 0.5 * (a(i, j + 1) - a(i, j - 1))
        end if
    end function

    subroutine gradient(a, g, n, m)
        real, intent(in) :: a(:,:)
        real, intent(out) :: g(:,:)
        integer, intent(in) :: n, m
        integer :: i, j
        do concurrent (j = 1:m, i = 1:n)
            g(i, j) = diff_x(a, n, i, j) + 2 * diff_y(a, m, i, j)
        end do
    end subroutine

    subroutine gradient_host(a, g, n, m)
        real, intent(in) :: a(:,:)
        real, intent(out) :: g(:,:)
        integer, intent(in) :: n, m
        integer :: i, j
        do j = 1, m
            do i = 1, n
                g(i, j) = diff_x(a, n, i, j) + 2 * diff_y(a, m, i, j)
            end do
        end do
    end subroutine

    ! Tests on both indices at once (a diagonal) and on a variable of the
    ! loop body stay as they are.
    subroutine diagonal(a, g, n, m, k)
        real, intent(in) :: a(:,:)
        real, intent(out) :: g(:,:)
        integer, intent(in) :: n, m, k
        integer :: i, j, s
        do concurrent (j = 1:m, i = 1:n) local(s)
            s = mod(i * j, 3)
            if (i == j) then
                g(i, j) = 1
            else if (s == k) then
                g(i, j) = diff_y(a, m, i, j)
            else
                g(i, j) = diff_x(a, n, i, j)
            end if
        end do
    end subroutine

    subroutine line(a, b, n)
        real, intent(in) :: a(:)
        real, intent(out) :: b(:)
        integer, intent(in) :: n
        integer :: i
        do concurrent (i = 1:n)
            if (i == 1) then
                b(i) = a(1)
            else if (i == n) then
                b(i) = a(n)
            else
                b(i) = 0.25 * a(i - 1) + 0.5 * a(i) + 0.25 * a(i + 1)
            end if
        end do
    end subroutine
end module

program gpu_metal_511
    ! Splitting a loop into an interior without the boundary tests and a
    ! frame around it gives the results of the loop, for domains with and
    ! without an interior.
    use gpu_metal_511_m
    implicit none
    integer :: sizes(2, 7), c, n, m, i, j
    real, allocatable :: a(:,:), g(:,:), h(:,:), x(:), y(:)
    sizes = reshape([40, 9, 5, 4, 4, 3, 3, 4, 7, 2, 64, 1, 1, 1], [2, 7])
    do c = 1, 7
        n = sizes(1, c)
        m = sizes(2, c)
        if (n < 3 .or. m < 2) then
            ! diff_x reads a(3, j) and diff_y a(i, 2).
            n = max(n, 3)
            m = max(m, 2)
        end if
        allocate(a(n, m), g(n, m), h(n, m))
        do j = 1, m
            do i = 1, n
                a(i, j) = real(mod(i * 7 + j * 13, 17)) + 0.1 * i * j
            end do
        end do
        call gradient(a, g, n, m)
        call gradient_host(a, h, n, m)
        if (maxval(abs(g - h)) > 1.0e-4 * maxval(abs(h))) error stop 1
        call diagonal(a, g, n, m, 1)
        do j = 1, m
            do i = 1, n
                if (i == j) then
                    if (g(i, j) /= 1) error stop 2
                else if (mod(i * j, 3) == 1) then
                    if (abs(g(i, j) - diff_y(a, m, i, j)) > 1e-4) error stop 3
                else
                    if (abs(g(i, j) - diff_x(a, n, i, j)) > 1e-4) error stop 4
                end if
            end do
        end do
        deallocate(a, g, h)
    end do
    do n = 1, 6
        allocate(x(n), y(n))
        x = [(real(i * i), i = 1, n)]
        call line(x, y, n)
        do i = 1, n
            if (i == 1) then
                if (y(i) /= x(1)) error stop 5
            else if (i == n) then
                if (y(i) /= x(n)) error stop 6
            else
                if (abs(y(i) - (0.25 * x(i - 1) + 0.5 * x(i) &
                    + 0.25 * x(i + 1))) > 1e-4) error stop 7
            end if
        end do
        deallocate(x, y)
    end do
    print *, "ok"
end program
