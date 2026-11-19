module gpu_lanes_split_02_m
! Rows that follow the sums an iteration's lanes combine and read an element
! of an array the loop reduces (gpu_vector_lanes.cpp): the iteration's
! contribution to the reduction is a scalar only the first lane holds,
! which the combination takes from it alone, so the rows that read it stay
! on the first lane rather than being shared out among the lanes, and the
! four rows of an iteration read the same value.
use, intrinsic :: iso_fortran_env, only: int16, int32
implicit none
contains
    pure real function bf(v)
        integer(int16), intent(in) :: v
        bf = transfer(shiftl(int(v, int32), 16), 0.0)
    end function

    pure real function gate(x)
        real, intent(in) :: x
        gate = merge(x, 0.5*x, x > 0)
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

    ! The iteration adds 1 to ss(1), then x(n+g) += a_g*ss(1) for its four rows.
    subroutine gemv_count(w, s, b, v, x, ss)
        integer(int32), intent(in) :: w(:, :)
        integer(int16), intent(in) :: s(:, :), b(:, :)
        real, intent(in) :: v(:)
        real, intent(inout) :: x(:), ss(:)
        integer :: i
        do concurrent (i = 1:size(x)/4) reduce(+: ss)
            block
                real :: a1, a2, a3, a4
                integer :: c, n
                n = 4*i - 3
                a1 = 0; a2 = 0; a3 = 0; a4 = 0
                do c = 1, size(w, 1)/4
                    a1 = a1 + qdot(w, s, b, v, n, c)
                    a2 = a2 + qdot(w, s, b, v, n + 1, c)
                    a3 = a3 + qdot(w, s, b, v, n + 2, c)
                    a4 = a4 + qdot(w, s, b, v, n + 3, c)
                end do
                ss(1) = ss(1) + 1
                x(n) = x(n) + a1*ss(1)
                x(n + 1) = x(n + 1) + a2*ss(1)
                x(n + 2) = x(n + 2) + a3*ss(1)
                x(n + 3) = x(n + 3) + a4*ss(1)
            end block
        end do
    end subroutine
end module

program gpu_lanes_split_02
use gpu_lanes_split_02_m
implicit none
integer, parameter :: k = 512, rows = 64
integer(int32) :: w(k/4, rows)
integer(int16) :: s(k/64, rows), b(k/64, rows)
real :: v(k), x(rows), ss(1), d(rows), m
integer :: i, j, c

do j = 1, rows
    do i = 1, k/4
        w(i, j) = ior(ior(mod(i + j, 3), shiftl(mod(i + 2*j, 3), 8)), &
            ior(shiftl(mod(2*i + j, 3), 16), shiftl(mod(i, 3), 24)))
    end do
    do i = 1, k/64
        s(i, j) = int(shiftr(transfer(real(mod(i + j, 3) + 1), 0_int32), 16), int16)
        b(i, j) = 0
    end do
end do
do i = 1, k
    v(i) = mod(i, 4) + 1
end do
do j = 1, rows
    d(j) = 0
    do c = 1, k/16
        d(j) = d(j) + qdot(w, s, b, v, j, c)
    end do
    if (d(j) == 0) error stop 1
end do
x = 0
ss = 0
call gemv_count(w, s, b, v, x, ss)
if (ss(1) /= rows/4) then
    print *, "ss", ss(1)
    error stop 2
end if
do i = 1, rows/4
    ! The multiplier the four rows of iteration i read.
    m = x(4*i - 3)/d(4*i - 3)
    if (m < 1) error stop 3
    do j = 4*i - 2, 4*i
        if (x(j) /= m*d(j)) then
            print *, "row", j, x(j)/d(j), m
            error stop 4
        end if
    end do
end do
print *, "ok"
end program
