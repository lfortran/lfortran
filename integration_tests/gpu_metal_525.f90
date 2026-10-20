module gpu_metal_525_m
    ! Index-set splitting (and unroll-and-jam) of loops whose indices are
    ! integer(8): the interior, the frame and the jammed rows are computed
    ! in the kind of the indices.
    implicit none
contains
    subroutine line(a, d, m)
        integer(8), intent(in) :: m
        real, intent(in) :: a(:)
        real, intent(out) :: d(:)
        integer(8) :: i
        do concurrent (i = 1:m)
            if (i == 1) then
                d(i) = a(2) - a(1)
            else if (i == m) then
                d(i) = a(m) - a(m - 1)
            else
                d(i) = 0.5 * (a(i + 1) - a(i - 1))
            end if
        end do
    end subroutine

    ! The same through a pure function of the whole array, inlined with
    ! the subscripts shifted by the lower bound of the actual argument.
    pure real function centred(a, i, m) result(r)
        real, intent(in) :: a(:)
        integer(8), intent(in) :: i, m
        if (i == 1) then
            r = a(2) - a(1)
        else if (i == m) then
            r = a(m) - a(m - 1)
        else
            r = 0.5 * (a(i + 1) - a(i - 1))
        end if
    end function

    subroutine line_call(a, d, m)
        integer(8), intent(in) :: m
        real, intent(in) :: a(:)
        real, intent(out) :: d(:)
        integer(8) :: i
        do concurrent (i = 1:m)
            d(i) = centred(a, i, m)
        end do
    end subroutine

    ! Reads its input at two offsets along the outer dimension j: jammed.
    subroutine smooth(a, b, n, m)
        integer(8), intent(in) :: n, m
        real, intent(in) :: a(:,:)
        real, intent(out) :: b(:,:)
        integer(8) :: i, j
        do concurrent (j = 1:m, i = 1:n)
            if (j == 1) then
                b(i, j) = a(i, 2) - a(i, 1)
            else if (j == m) then
                b(i, j) = a(i, m) - a(i, m - 1)
            else if (i == 1 .or. i == n) then
                b(i, j) = a(i, j)
            else
                b(i, j) = 0.25 * (a(i - 1, j) + a(i + 1, j)) &
                    + 0.5 * (a(i, j + 1) - a(i, j - 1))
            end if
        end do
    end subroutine

    real function expected(a, i, j, n, m) result(e)
        real, intent(in) :: a(:,:)
        integer(8), intent(in) :: i, j, n, m
        if (j == 1) then
            e = a(i, 2) - a(i, 1)
        else if (j == m) then
            e = a(i, m) - a(i, m - 1)
        else if (i == 1 .or. i == n) then
            e = a(i, j)
        else
            e = 0.25 * (a(i - 1, j) + a(i + 1, j)) &
                + 0.5 * (a(i, j + 1) - a(i, j - 1))
        end if
    end function
end module

program gpu_metal_525
    use gpu_metal_525_m
    implicit none
    integer(8) :: sizes(2, 4), c, n, m, i, j
    real, allocatable :: a(:,:), b(:,:), x(:), y(:)
    real :: e
    do m = 2, 40
        allocate(x(m), y(m))
        do i = 1, m
            x(i) = real(i * i)
        end do
        call line(x, y, m)
        if (abs(y(1) - 3) > 1e-4) error stop 1
        if (abs(y(m) - (x(m) - x(m - 1))) > 1e-3) error stop 2
        do i = 2, m - 1
            if (abs(y(i) - 2 * i) > 1e-3) error stop 3
        end do
        y = 0
        call line_call(x, y, m)
        if (abs(y(1) - 3) > 1e-4) error stop 5
        if (abs(y(m) - (x(m) - x(m - 1))) > 1e-3) error stop 6
        do i = 2, m - 1
            if (abs(y(i) - 2 * i) > 1e-3) error stop 7
        end do
        deallocate(x, y)
    end do
    sizes = reshape([13_8, 11_8, 40_8, 9_8, 5_8, 4_8, 3_8, 2_8], [2, 4])
    do c = 1, 4
        n = sizes(1, c)
        m = sizes(2, c)
        allocate(a(n, m), b(n, m))
        do j = 1, m
            do i = 1, n
                a(i, j) = real(mod(i * 7 + j * 13, 17_8))
            end do
        end do
        call smooth(a, b, n, m)
        do j = 1, m
            do i = 1, n
                e = expected(a, i, j, n, m)
                if (abs(b(i, j) - e) > 1e-4) error stop 4
            end do
        end do
        deallocate(a, b)
    end do
    print *, "ok"
end program
