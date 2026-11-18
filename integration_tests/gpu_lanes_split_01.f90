module gpu_lanes_split_01_m
! Statements that follow the sums an iteration's lanes combine, which the
! first lane ran one after another, are shared out: lane g runs the statements
! of row g of the iteration. The statements of a row may depend on each other
! (x, then xw, which reads it), and one that reads every row (the sum of the
! new x) waits for the lanes and runs on the first. An `if` whose test every
! lane evaluates alike keeps its branches shared out; rows that depend on each
! other stay on the first lane. Rows that only store what the lanes hold
! (no load to wait for) stay on the first lane as well.
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

    ! x += W v; xw = x*wn; t(i) = the sum of the four new rows of x.
    subroutine gemv_resid(w, s, b, v, wn, x, xw, t)
        integer(int32), intent(in) :: w(:, :)
        integer(int16), intent(in) :: s(:, :), b(:, :)
        real, intent(in) :: v(:), wn(:)
        real, intent(inout) :: x(:), xw(:)
        real, intent(out) :: t(:)
        integer :: i
        do concurrent (i = 1:size(x)/4)
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
                x(n) = x(n) + a1
                x(n + 1) = x(n + 1) + a2
                x(n + 2) = x(n + 2) + a3
                x(n + 3) = x(n + 3) + a4
                xw(n) = x(n)*wn(n)
                xw(n + 1) = x(n + 1)*wn(n + 1)
                xw(n + 2) = x(n + 2)*wn(n + 2)
                xw(n + 3) = x(n + 3)*wn(n + 3)
                t(i) = (x(n) + x(n + 1)) + (x(n + 2) + x(n + 3))
            end block
        end do
    end subroutine

    ! y += r*(W v), or with `gated` the rows in pairs: y(2i-1) += gate(r*a1)*(r*a3),
    ! y(2i) += gate(r*a2)*(r*a4). A temporary of a row (a scalar assigned
    ! again by the next row, a local of a block) is the lane's own. The first
    ! iteration alone counts the launches.
    subroutine gemv_gate(w, s, b, v, r, gated, y, launches)
        integer(int32), intent(in) :: w(:, :)
        integer(int16), intent(in) :: s(:, :), b(:, :)
        real, intent(in) :: v(:), r
        logical, intent(in) :: gated
        real, intent(inout) :: y(:)
        integer, intent(inout) :: launches(:)
        integer :: i
        do concurrent (i = 1:size(w, 2)/4)
            block
                real :: a1, a2, a3, a4, t
                integer :: c, n
                n = 4*i - 3
                a1 = 0; a2 = 0; a3 = 0; a4 = 0
                do c = 1, size(w, 1)/4
                    a1 = a1 + qdot(w, s, b, v, n, c)
                    a2 = a2 + qdot(w, s, b, v, n + 1, c)
                    a3 = a3 + qdot(w, s, b, v, n + 2, c)
                    a4 = a4 + qdot(w, s, b, v, n + 3, c)
                end do
                if (gated) then
                    block
                        real :: g
                        g = gate(a1*r)
                        y(2*i - 1) = y(2*i - 1) + g*(a3*r)
                    end block
                    block
                        real :: g
                        g = gate(a2*r)
                        y(2*i) = y(2*i) + g*(a4*r)
                    end block
                else
                    t = a1*r
                    y(n) = y(n) + t
                    t = a2*r
                    y(n + 1) = y(n + 1) + t
                    t = a3*r
                    y(n + 2) = y(n + 2) + t
                    t = a4*r
                    y(n + 3) = y(n + 3) + t
                end if
                if (i == 1) launches(1) = launches(1) + 1
            end block
        end do
    end subroutine

    ! Each row adds the one before: the rows stay on the first lane.
    subroutine gemv_chain(w, s, b, v, y)
        integer(int32), intent(in) :: w(:, :)
        integer(int16), intent(in) :: s(:, :), b(:, :)
        real, intent(in) :: v(:)
        real, intent(out) :: y(:)
        integer :: i
        do concurrent (i = 1:size(y)/4)
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
                y(n) = a1
                y(n + 1) = y(n) + a2
                y(n + 2) = y(n + 1) + a3
                y(n + 3) = y(n + 2) + a4
            end block
        end do
    end subroutine

    ! Three columns per iteration, on the lanes of a warp when the launch has
    ! few iterations and on one lane when it has many.
    subroutine colsum3(a, y)
        real, intent(in) :: a(:, :)
        real, intent(out) :: y(:)
        integer :: i
        do concurrent (i = 1:size(y)/3)
            block
                real :: s1, s2, s3
                integer :: k
                s1 = 0; s2 = 0; s3 = 0
                do k = 1, size(a, 1)
                    s1 = s1 + a(k, 3*i - 2)
                    s2 = s2 + a(k, 3*i - 1)
                    s3 = s3 + a(k, 3*i)
                end do
                y(3*i - 2) = y(3*i - 2) + 2*s1
                y(3*i - 1) = y(3*i - 1) + 2*s2
                y(3*i) = y(3*i) + 2*s3
            end block
        end do
    end subroutine
end module

program gpu_lanes_split_01
! The scales and biases are powers of two and the vector is integer-valued,
! so every sum is exact in any order.
use, intrinsic :: iso_fortran_env, only: int16, int32
use gpu_lanes_split_01_m
implicit none
integer, parameter :: k = 512, rows = 48, many = 3*70000
integer(int32) :: w(k/4, rows)
integer(int16) :: s(k/64, rows), b(k/64, rows)
real :: v(k), wn(rows), x(rows), xw(rows), t(rows/4), y(rows), yg(rows/2), yc(rows)
real :: ref(rows), x0(rows), r
real, allocatable :: a(:, :), ys(:), yl(:)
integer :: launches(1), i, j, c, q
do j = 1, rows
    do i = 1, k/4
        w(i, j) = 0
        do q = 0, 3
            w(i, j) = ior(w(i, j), shiftl(mod(3*i + 5*j + 7*q, 251), 8*q))
        end do
    end do
    do i = 1, k/64
        ! bf16 of 2**e: the exponent field is e + 127.
        s(i, j) = int(shiftl(127 + mod(i + j, 3) - 1, 7), int16)
        b(i, j) = int(shiftl(127 + mod(i, 2), 7) - 32768, int16)
    end do
    wn(j) = real(mod(j, 3) + 1)
    x0(j) = real(mod(7*j, 11) - 5)
end do
do i = 1, k
    v(i) = real(mod(i, 7) - 3)
end do
do j = 1, rows
    ref(j) = 0
    do c = 1, k/16
        ref(j) = ref(j) + 2.0**(mod((c - 1)/4 + 1 + j, 3) - 1)*part(c, j) &
            - 2.0**mod((c - 1)/4 + 1, 2)*vsum(c)
    end do
end do

x = x0
call gemv_resid(w, s, b, v, wn, x, xw, t)
do j = 1, rows
    if (x(j) /= x0(j) + ref(j)) error stop 1
    if (xw(j) /= (x0(j) + ref(j))*wn(j)) error stop 2
end do
do i = 1, rows/4
    if (t(i) /= (x(4*i - 3) + x(4*i - 2)) + (x(4*i - 1) + x(4*i))) error stop 3
end do

r = 0.5
launches = 0
y = 0
call gemv_gate(w, s, b, v, r, .false., y, launches)
do j = 1, rows
    if (y(j) /= ref(j)*r) error stop 4
end do
yg = 0
call gemv_gate(w, s, b, v, r, .true., yg, launches)
do i = 1, rows/4
    if (yg(2*i - 1) /= gate(ref(4*i - 3)*r)*(ref(4*i - 1)*r)) error stop 5
    if (yg(2*i) /= gate(ref(4*i - 2)*r)*(ref(4*i)*r)) error stop 6
end do
if (launches(1) /= 2) error stop 7

call gemv_chain(w, s, b, v, yc)
do i = 1, rows/4
    if (yc(4*i - 3) /= ref(4*i - 3)) error stop 8
    if (yc(4*i - 2) /= yc(4*i - 3) + ref(4*i - 2)) error stop 9
    if (yc(4*i - 1) /= yc(4*i - 2) + ref(4*i - 1)) error stop 10
    if (yc(4*i) /= yc(4*i - 1) + ref(4*i)) error stop 11
end do

! A launch of few iterations and one of many.
allocate(a(5, many), ys(30), yl(many))
do j = 1, many
    do i = 1, 5
        a(i, j) = real(mod(i*j, 13))
    end do
end do
ys = 1
yl = 1
call colsum3(a(:, 1:30), ys)
call colsum3(a, yl)
do j = 1, 30
    if (ys(j) /= 2*sum(a(:, j)) + 1) error stop 12
end do
do j = 1, many
    if (yl(j) /= 2*sum(a(:, j)) + 1) error stop 13
end do
print *, "ok"

contains

    real function part(c, j)
        integer, intent(in) :: c, j
        integer :: i, q
        part = 0
        do i = 0, 15
            q = 4*c - 3 + i/4
            part = part + real(iand(shiftr(w(q, j), 8*mod(i, 4)), 255))*v(16*c - 15 + i)
        end do
    end function

    real function vsum(c)
        integer, intent(in) :: c
        vsum = sum(v(16*c - 15:16*c))
    end function
end program
