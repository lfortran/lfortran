module gpu_wide_rows_01_m
use, intrinsic :: iso_fortran_env, only: int16, int32
implicit none
contains
    pure real function bf(v)
        integer(int16), intent(in) :: v
        bf = transfer(shiftl(int(v, int32), 16), 0.0)
    end function

    ! One chunk c (16 bytes) of row n of a q8 matrix, dotted with x:
    ! scale*sum(byte*x) + bias*sum(x), one bf16 scale and bias per 4 chunks.
    pure real function qdot(w, s, b, x, n, c)
        integer(int32), intent(in) :: w(:, :)
        integer(int16), intent(in) :: s(:, :), b(:, :)
        real, intent(in) :: x(:)
        integer, intent(in) :: n, c
        qdot = bf(s((c-1)/4+1, n))*(((transfer(ior(iand(w(4*c-3, n), 255), 1258291200), 0.0) - 8388608.0)*x(16*c-15) &
            + (transfer(ior(iand(shiftr(w(4*c-3, n), 8), 255), 1258291200), 0.0) - 8388608.0)*x(16*c-14) &
            + (transfer(ior(iand(shiftr(w(4*c-3, n), 16), 255), 1258291200), 0.0) - 8388608.0)*x(16*c-13) &
            + (transfer(ior(shiftr(w(4*c-3, n), 24), 1258291200), 0.0) - 8388608.0)*x(16*c-12)) &
            + ((transfer(ior(iand(w(4*c-2, n), 255), 1258291200), 0.0) - 8388608.0)*x(16*c-11) &
            + (transfer(ior(iand(shiftr(w(4*c-2, n), 8), 255), 1258291200), 0.0) - 8388608.0)*x(16*c-10) &
            + (transfer(ior(iand(shiftr(w(4*c-2, n), 16), 255), 1258291200), 0.0) - 8388608.0)*x(16*c-9) &
            + (transfer(ior(shiftr(w(4*c-2, n), 24), 1258291200), 0.0) - 8388608.0)*x(16*c-8)) &
            + ((transfer(ior(iand(w(4*c-1, n), 255), 1258291200), 0.0) - 8388608.0)*x(16*c-7) &
            + (transfer(ior(iand(shiftr(w(4*c-1, n), 8), 255), 1258291200), 0.0) - 8388608.0)*x(16*c-6) &
            + (transfer(ior(iand(shiftr(w(4*c-1, n), 16), 255), 1258291200), 0.0) - 8388608.0)*x(16*c-5) &
            + (transfer(ior(shiftr(w(4*c-1, n), 24), 1258291200), 0.0) - 8388608.0)*x(16*c-4)) &
            + ((transfer(ior(iand(w(4*c, n), 255), 1258291200), 0.0) - 8388608.0)*x(16*c-3) &
            + (transfer(ior(iand(shiftr(w(4*c, n), 8), 255), 1258291200), 0.0) - 8388608.0)*x(16*c-2) &
            + (transfer(ior(iand(shiftr(w(4*c, n), 16), 255), 1258291200), 0.0) - 8388608.0)*x(16*c-1) &
            + (transfer(ior(shiftr(w(4*c, n), 24), 1258291200), 0.0) - 8388608.0)*x(16*c))) &
            + bf(b((c-1)/4+1, n))*(((x(16*c-15) + x(16*c-14)) + (x(16*c-13) + x(16*c-12))) &
            + ((x(16*c-11) + x(16*c-10)) + (x(16*c-9) + x(16*c-8))) &
            + ((x(16*c-7) + x(16*c-6)) + (x(16*c-5) + x(16*c-4))) &
            + ((x(16*c-3) + x(16*c-2)) + (x(16*c-1) + x(16*c))))
    end function

    ! Four rows per iteration: on an NVIDIA GPU each iteration runs on the
    ! 64 lanes of a block, which combine the four sums together.
    subroutine gemv4(w, s, b, x, y)
        integer(int32), intent(in) :: w(:, :)
        integer(int16), intent(in) :: s(:, :), b(:, :)
        real, intent(in) :: x(:)
        real, intent(out) :: y(:)
        integer :: i
        do concurrent (i = 1:size(y)/4)
            block
                real :: a1, a2, a3, a4
                integer :: c, n
                n = 4*i - 3
                a1 = 0; a2 = 0; a3 = 0; a4 = 0
                do c = 1, size(w, 1)/4
                    a1 = a1 + qdot(w, s, b, x, n, c)
                    a2 = a2 + qdot(w, s, b, x, n + 1, c)
                    a3 = a3 + qdot(w, s, b, x, n + 2, c)
                    a4 = a4 + qdot(w, s, b, x, n + 3, c)
                end do
                y(n) = a1; y(n + 1) = a2; y(n + 2) = a3; y(n + 3) = a4
            end block
        end do
    end subroutine

    ! Two rows per iteration, on the 32 lanes of a warp.
    subroutine gemv2(w, s, b, x, y)
        integer(int32), intent(in) :: w(:, :)
        integer(int16), intent(in) :: s(:, :), b(:, :)
        real, intent(in) :: x(:)
        real, intent(out) :: y(:)
        integer :: i
        do concurrent (i = 1:size(y)/2)
            block
                real :: a1, a2
                integer :: c, n
                n = 2*i - 1
                a1 = 0; a2 = 0
                do c = 1, size(w, 1)/4
                    a1 = a1 + qdot(w, s, b, x, n, c)
                    a2 = a2 + qdot(w, s, b, x, n + 1, c)
                end do
                y(n) = a1; y(n + 1) = a2
            end block
        end do
    end subroutine
end module

program gpu_wide_rows_01
! The scales and biases are powers of two and the vector is integer-valued,
! so every sum is exact in any order.
use, intrinsic :: iso_fortran_env, only: int16, int32
use gpu_wide_rows_01_m, only: gemv4, gemv2
implicit none
integer, parameter :: k = 2048, rows = 40
integer(int32) :: w(k/4, rows)
integer(int16) :: s(k/64, rows), b(k/64, rows)
real :: x(k), y4(rows), y2(rows), ref, sc, bi, part, xs
integer :: i, j, c, t
do j = 1, rows
    do i = 1, k/4
        w(i, j) = 0
        do t = 0, 3
            w(i, j) = ior(w(i, j), shiftl(mod(3*i + 5*j + 7*t, 251), 8*t))
        end do
    end do
    do i = 1, k/64
        ! bf16 of 2**e: the exponent field is e + 127.
        s(i, j) = int(shiftl(127 + mod(i + j, 3) - 1, 7), int16)
        b(i, j) = int(shiftl(127 + mod(i, 2), 7) - 32768, int16)
    end do
end do
do i = 1, k
    x(i) = real(mod(i, 7) - 3)
end do
call gemv4(w, s, b, x, y4)
call gemv2(w, s, b, x, y2)
do j = 1, rows
    ref = 0
    do c = 1, k/16
        sc = 2.0**(mod((c - 1)/4 + 1 + j, 3) - 1)
        bi = -(2.0**mod((c - 1)/4 + 1, 2))
        part = 0
        xs = 0
        do i = 0, 15
            t = 4*c - 3 + i/4
            part = part + real(iand(shiftr(w(t, j), 8*mod(i, 4)), 255))*x(16*c - 15 + i)
            xs = xs + x(16*c - 15 + i)
        end do
        ref = ref + sc*part + bi*xs
    end do
    if (y4(j) /= ref) error stop 1
    if (y2(j) /= ref) error stop 2
end do
print *, "ok"
end program
