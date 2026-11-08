module gpu_inline_loop_calls_01_m
    use, intrinsic :: iso_fortran_env, only: int16, real32
    implicit none
contains
    pure elemental function widen(x) result(y)
        integer(int16), intent(in) :: x
        real(real32) :: y
        y = real(x, real32)
    end function

    pure function sum_squares(x) result(total)
        integer(int16), intent(in) :: x(:)
        real(real32) :: total
        total = sum(widen(x)**2)
    end function

    pure function weighted(x, w) result(total)
        real(real32), intent(in) :: x(:), w(:)
        real(real32) :: total
        integer :: k
        total = 0
        do k = 1, size(x)
            total = total + x(k)*w(k)
        end do
    end function

    ! The shape of an RMS norm: a sum of squares through a function, then
    ! the row scaled by it.
    pure subroutine scale_rows(x, y, s)
        integer(int16), contiguous, intent(in) :: x(:, :)
        real(real32), contiguous, intent(out) :: y(:, :), s(:)
        integer :: m, d
        do concurrent (m = 1:size(x, 2))
            block
                real(real32) :: total
                total = sum_squares(x(:, m))
                s(m) = total
                do concurrent (d = 1:size(x, 1))
                    y(d, m) = widen(x(d, m))*total
                end do
            end block
        end do
    end subroutine

    pure subroutine dots(x, w, t)
        real(real32), contiguous, intent(in) :: x(:, :), w(:)
        real(real32), contiguous, intent(out) :: t(:)
        integer :: m
        do concurrent (m = 1:size(x, 2))
            t(m) = 2*weighted(x(:, m), w) + 1
        end do
    end subroutine
end module

program gpu_inline_loop_calls_01
! A kernel's calls of small pure functions that run a loop are inlined
! (gpu_inline_loop_calls.cpp), so that the lanes of an iteration share the
! loop and combine the sum. Every sum here is of integers, exact in any
! order.
use, intrinsic :: iso_fortran_env, only: int16, real32
use gpu_inline_loop_calls_01_m, only: scale_rows, dots
implicit none
integer, parameter :: n = 640, rows = 3
integer(int16) :: x(n, rows)
real(real32) :: y(n, rows), s(rows), a(n, rows), w(n), t(rows), ref
integer :: i, j
do j = 1, rows
    do i = 1, n
        x(i, j) = int(mod(i + 3*j, 7) - 3, int16)
        a(i, j) = real(mod(i*j, 5) - 2, real32)
    end do
end do
do i = 1, n
    w(i) = real(mod(i, 3), real32)
end do
call scale_rows(x, y, s)
call dots(a, w, t)
do j = 1, rows
    ref = 0
    do i = 1, n
        ref = ref + real(x(i, j), real32)**2
    end do
    if (s(j) /= ref) error stop "sum of squares"
    do i = 1, n
        if (y(i, j) /= real(x(i, j), real32)*ref) error stop "scaled row"
    end do
    ref = 0
    do i = 1, n
        ref = ref + a(i, j)*w(i)
    end do
    if (t(j) /= 2*ref + 1) error stop "weighted sum"
end do
print *, "ok"
end program
