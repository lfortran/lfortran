module gpu_inline_loop_calls_02_m
    use, intrinsic :: iso_fortran_env, only: int32, real32
    implicit none
contains
    ! The sum of the four bytes of a word, each times an element of x.
    pure real(real32) function dot4(w, x0, x1, x2, x3)
        integer(int32), intent(in) :: w
        real(real32), intent(in) :: x0, x1, x2, x3
        dot4 = iand(w, 255)*x0 + iand(shiftr(w, 8), 255)*x1 &
            + iand(shiftr(w, 16), 255)*x2 + shiftr(w, 24)*x3
    end function

    ! One chunk c (4 words, 16 bytes) of row n of w, dotted with x, times a
    ! scale, plus the sum of the chunk of x: one expression of array dummies,
    ! with no loop.
    pure real(real32) function qdot(w, s, x, n, c)
        integer(int32), intent(in) :: w(:, :)
        real(real32), intent(in) :: s(:), x(:)
        integer, intent(in) :: n, c
        qdot = s(n)*((dot4(w(4*c-3, n), x(16*c-15), x(16*c-14), x(16*c-13), x(16*c-12)) &
            + dot4(w(4*c-2, n), x(16*c-11), x(16*c-10), x(16*c-9), x(16*c-8))) &
            + (dot4(w(4*c-1, n), x(16*c-7), x(16*c-6), x(16*c-5), x(16*c-4)) &
            + dot4(w(4*c, n), x(16*c-3), x(16*c-2), x(16*c-1), x(16*c)))) &
            + ((x(16*c-15) + x(16*c-14)) + (x(16*c-13) + x(16*c-12)))
    end function

    pure integer function row(n)
        integer, intent(in) :: n
        row = n
    end function

    ! Two rows per iteration, each a sum of calls of qdot over the chunks.
    subroutine gemv2(w, s, x, y)
        integer(int32), intent(in) :: w(:, :)
        real(real32), intent(in) :: s(:), x(:)
        real(real32), intent(out) :: y(:)
        integer :: i
        do concurrent (i = 1:size(y)/2)
            block
                real(real32) :: a1, a2
                integer :: c, n
                a1 = 0; a2 = 0
                n = 2*i - 1
                do c = 1, size(w, 1)/4
                    a1 = a1 + qdot(w, s, x, n, c)
                    a2 = a2 + qdot(w, s, x, n + 1, c)
                end do
                y(n) = a1; y(n + 1) = a2
            end block
        end do
    end subroutine

    ! The row through a function call, which is not copied into each use.
    subroutine gemv1(w, s, x, y)
        integer(int32), intent(in) :: w(:, :)
        real(real32), intent(in) :: s(:), x(:)
        real(real32), intent(out) :: y(:)
        integer :: n
        do concurrent (n = 1:size(y))
            block
                real(real32) :: a
                integer :: c
                a = 0
                do c = 1, size(w, 1)/4
                    a = a + qdot(w, s, x, row(n), c)
                end do
                y(n) = a
            end block
        end do
    end subroutine
end module

program gpu_inline_loop_calls_02
! A kernel's calls of a small pure function of array dummies without a loop
! are inlined as the expression they compute (gpu_inline_loop_calls.cpp),
! so that the lanes of an iteration share the loop that sums them, and read
! the elements of the arrays side by side as vectors (gpu_wide_loads.cpp).
! The allocatable arrays of the program start at 0 and 3, which the elements
! of the inlined dummies are counted from. Every value is an integer, exact in any order.
use, intrinsic :: iso_fortran_env, only: int32, real32
use gpu_inline_loop_calls_02_m, only: qdot, gemv1, gemv2
implicit none
integer, parameter :: k = 1024, rows = 6
integer(int32) :: w(k/4, rows)
integer(int32), allocatable :: w0(:, :)
real(real32) :: s(rows), x(k), y(rows), y1(rows), y0(rows), ref
real(real32), allocatable :: x0(:)
integer :: i, j, c, b, n
do j = 1, rows
    s(j) = real(mod(j, 3) + 1, real32)
    do i = 1, k/4
        w(i, j) = 0
        do b = 0, 3
            w(i, j) = ior(w(i, j), shiftl(mod(i + 5*j + 7*b, 13), 8*b))
        end do
    end do
end do
do i = 1, k
    x(i) = real(mod(i, 5) - 2, real32)
end do
allocate(w0(0:k/4-1, 0:rows-1), x0(3:k+2))
w0 = w
x0 = x
call gemv2(w, s, x, y)
call gemv1(w, s, x, y1)
! The same sums over arrays that start at 0 and 3.
do concurrent (n = 1:rows)
    block
        real(real32) :: a
        integer :: cc
        a = 0
        do cc = 1, k/16
            a = a + qdot(w0, s, x0, n, cc)
        end do
        y0(n) = a
    end block
end do
do j = 1, rows
    ref = 0
    do c = 1, k/16
        do i = 1, 4
            do b = 0, 3
                ref = ref + s(j)*mod(4*c - 4 + i + 5*j + 7*b, 13) &
                    *x(16*c - 16 + 4*(i - 1) + b + 1)
            end do
        end do
        ref = ref + sum(x(16*c-15:16*c-12))
    end do
    if (y(j) /= ref) error stop "two rows per iteration"
    if (y1(j) /= ref) error stop "row through a call"
    if (y0(j) /= ref) error stop "arrays that start at 0 and 3"
end do
print *, "ok"
end program
