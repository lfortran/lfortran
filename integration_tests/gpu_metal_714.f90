program gpu_metal_714
    ! Jammed index-set split of a 2-D stencil (reads at j - 1 and j + 1)
    ! whose outer domain starts at or just above -huge(0): the jammed frame
    ! rows must fit the loop index's kind, otherwise the split is not taken.
    implicit none
    integer, parameter :: n = 8, m = 10
    real :: a(n, m), b(n, m), e
    integer :: i, j, lo, off
    do off = 0, 12
        lo = -2147483647 + off
        call fill()
        b = -1.0
        call smooth(lo, lo + m - 1)
        call check()
    end do
    print *, "ok"
contains
    subroutine fill()
        do j = 1, m
            do i = 1, n
                a(i, j) = real(mod(i * 7 + j * 13, 17))
            end do
        end do
    end subroutine
    subroutine smooth(l, h)
        integer, intent(in) :: l, h
        integer(8) :: o
        o = int(l, 8) - 1_8
        do concurrent (j = l:h, i = 1:n)
            if (j == l) then
                b(i, j - o) = a(i, 2) - a(i, 1)
            else if (j == h) then
                b(i, j - o) = a(i, m) - a(i, m - 1)
            else if (i == 1 .or. i == n) then
                b(i, j - o) = a(i, j - o)
            else
                b(i, j - o) = 0.25 * (a(i - 1, j - o) + a(i + 1, j - o)) &
                    + 0.5 * (a(i, j - o + 1) - a(i, j - o - 1))
            end if
        end do
    end subroutine
    subroutine check()
        do j = 1, m
            do i = 1, n
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
                if (abs(b(i, j) - e) > 1e-5) then
                    print *, "off", off, i, j, b(i, j), e
                    error stop 1
                end if
            end do
        end do
    end subroutine
end program
