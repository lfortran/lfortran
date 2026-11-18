module gpu_overlap_trigger_01_m
! With --gpu-overlap a kernel whose main loop is its first access to memory
! (here the rows of a q8 matrix-vector product) lets the next kernel start
! after that loop, before it stores its results: the next kernel must not
! read or write memory before the product is done. It writes the vector the
! product reads at every step, and now and then the matrix, which the next
! product reads.
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

    subroutine update(v, y)
        real, intent(inout) :: v(:)
        real, intent(in) :: y(:)
        integer :: i
        do concurrent (i = 1:size(v))
            v(i) = real(modulo(nint(v(i)) + nint(y(mod(i - 1, size(y)) + 1)), 7) - 3)
        end do
    end subroutine

    subroutine flip(w, step)
        integer(int32), intent(inout) :: w(:, :)
        integer, intent(in) :: step
        integer :: j
        do concurrent (j = 1:size(w, 2))
            w(mod(step, size(w, 1)) + 1, j) = ieor(w(mod(step, size(w, 1)) + 1, j), 1)
        end do
    end subroutine
end module

program gpu_overlap_trigger_01
use, intrinsic :: iso_fortran_env, only: int16, int32
use gpu_overlap_trigger_01_m
implicit none
integer, parameter :: k = 512, rows = 4*600, steps = 60
integer(int32) :: w(k/4, rows), hw(k/4, rows)
integer(int16) :: s(k/64, rows), b(k/64, rows)
real :: v(k), hv(k), y(rows), hy(rows)
integer :: i, j, t, step
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
    v(i) = real(mod(i, 7) - 3)
end do
hw = w
hv = v
do step = 1, steps
    call gemv4(w, s, b, v, y)
    call update(v, y)
    if (mod(step, 7) == 0) call flip(w, step)
end do
do step = 1, steps
    call host_gemv(hw, hv, hy)
    do i = 1, k
        hv(i) = real(modulo(nint(hv(i)) + nint(hy(mod(i - 1, rows) + 1)), 7) - 3)
    end do
    if (mod(step, 7) == 0) then
        hw(mod(step, k/4) + 1, :) = ieor(hw(mod(step, k/4) + 1, :), 1)
    end if
end do
if (any(y /= hy)) error stop 1
if (any(v /= hv)) error stop 2
if (any(w /= hw)) error stop 3
print *, sum(y), sum(v)

contains

    subroutine host_gemv(w, x, y)
        integer(int32), intent(in) :: w(:, :)
        real, intent(in) :: x(:)
        real, intent(out) :: y(:)
        integer :: i, j, c, t
        real :: part, xs
        do j = 1, size(y)
            y(j) = 0
            do c = 1, size(x)/16
                part = 0
                xs = 0
                do i = 0, 15
                    t = 4*c - 3 + i/4
                    part = part + real(iand(shiftr(w(t, j), 8*mod(i, 4)), 255))*x(16*c - 15 + i)
                    xs = xs + x(16*c - 15 + i)
                end do
                y(j) = y(j) + 2.0**(mod((c - 1)/4 + 1 + j, 3) - 1)*part &
                    - 2.0**mod((c - 1)/4 + 1, 2)*xs
            end do
        end do
    end subroutine
end program
