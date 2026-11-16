module gpu_wide_loads_hoist_01_m
use, intrinsic :: iso_fortran_env, only: int32
implicit none
contains
    pure real function dot4(w, x0, x1, x2, x3)
        integer(int32), intent(in) :: w
        real, intent(in) :: x0, x1, x2, x3
        dot4 = iand(w, 255)*x0 + iand(shiftr(w, 8), 255)*x1 &
            + iand(shiftr(w, 16), 255)*x2 + shiftr(w, 24)*x3
    end function

    ! A loop whose statements read vectors (the words of a row and the
    ! elements of x) and scalars (the scales of the row): on an NVIDIA GPU
    ! the vectors, and the scales whose subscripts nothing before them
    ! assigns, are read at the start of the body (gpu_wide_loads.cpp). The
    ! scale of a group is read where it is, after `g` is assigned.
    subroutine scaled(w, s, x, y)
        integer(int32), intent(in) :: w(:, :)
        real, intent(in) :: s(:, :), x(:)
        real, intent(out) :: y(:)
        integer :: n
        do concurrent (n = 1:size(y))
            block
                real :: a
                integer :: c, g
                a = 0
                do c = 1, size(w, 1)/4
                    g = (c + 1)/2
                    a = a + s(g, n)*(dot4(w(4*c-3, n), x(16*c-15), x(16*c-14), x(16*c-13), x(16*c-12)) &
                        + dot4(w(4*c-2, n), x(16*c-11), x(16*c-10), x(16*c-9), x(16*c-8)) &
                        + dot4(w(4*c-1, n), x(16*c-7), x(16*c-6), x(16*c-5), x(16*c-4)) &
                        + dot4(w(4*c, n), x(16*c-3), x(16*c-2), x(16*c-1), x(16*c)))
                    a = a + s(1, n)
                end do
                y(n) = a
            end block
        end do
    end subroutine

    ! The same sum up to the first chunk whose first word is negative: the
    ! loop may leave before a statement, so nothing is read ahead of it.
    subroutine until_negative(w, x, y)
        integer(int32), intent(in) :: w(:, :)
        real, intent(in) :: x(:)
        real, intent(out) :: y(:)
        integer :: n
        do concurrent (n = 1:size(y))
            block
                real :: a
                integer :: c
                a = 0
                do c = 1, size(w, 1)/4
                    if (w(4*c-3, n) < 0) exit
                    a = a + dot4(w(4*c-3, n), x(16*c-15), x(16*c-14), x(16*c-13), x(16*c-12)) &
                        + dot4(w(4*c-2, n), x(16*c-11), x(16*c-10), x(16*c-9), x(16*c-8)) &
                        + dot4(w(4*c-1, n), x(16*c-7), x(16*c-6), x(16*c-5), x(16*c-4)) &
                        + dot4(w(4*c, n), x(16*c-3), x(16*c-2), x(16*c-1), x(16*c))
                end do
                y(n) = a
            end block
        end do
    end subroutine
end module

program gpu_wide_loads_hoist_01
! Every value is an integer, so the sums are exact in any order.
use, intrinsic :: iso_fortran_env, only: int32
use gpu_wide_loads_hoist_01_m, only: scaled, until_negative
implicit none
integer, parameter :: k = 1024, rows = 5
integer(int32) :: w(k/4, rows)
real :: s(k/32, rows), x(k), y(rows), z(rows), ref, part
integer :: i, j, c, b
do j = 1, rows
    do i = 1, k/4
        w(i, j) = 0
        do b = 0, 3
            w(i, j) = ior(w(i, j), shiftl(mod(i + 5*j + 7*b, 13), 8*b))
        end do
    end do
    do i = 1, k/32
        s(i, j) = real(mod(i + j, 3) + 1)
    end do
end do
! A negative first word in row 2 and row 4, in the middle.
w(4*20 - 3, 2) = -1
w(4*41 - 3, 4) = -7
do i = 1, k
    x(i) = real(mod(i, 5) - 2)
end do
call scaled(w, s, x, y)
call until_negative(w, x, z)
do j = 1, rows
    ref = 0
    do c = 1, k/16
        part = 0
        do i = 1, 4
            do b = 0, 3
                part = part + real(iand(shiftr(w(4*c-4+i, j), 8*b), 255))*x(16*c-16+4*(i-1)+b+1)
            end do
        end do
        ref = ref + s((c + 1)/2, j)*part + s(1, j)
    end do
    if (y(j) /= ref) error stop 1
    ref = 0
    do c = 1, k/16
        if (w(4*c-3, j) < 0) exit
        do i = 1, 4
            do b = 0, 3
                ref = ref + real(iand(shiftr(w(4*c-4+i, j), 8*b), 255))*x(16*c-16+4*(i-1)+b+1)
            end do
        end do
    end do
    if (z(j) /= ref) error stop 2
end do
print *, "ok"
end program
