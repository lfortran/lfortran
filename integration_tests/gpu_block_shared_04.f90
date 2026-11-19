module gpu_block_shared_04_m
implicit none
integer, parameter :: n = 128, m = 64
contains

! The state update of a gated delta rule, one head per iteration: a phase
! over the columns of the head's state in which each column takes the inner
! product of the column with `k`, then updates the column and takes its
! inner product with `q`. The iteration runs on the lanes of a block, and
! the arrays of the iteration live in the block's shared memory
! (gpu_vector_lanes.cpp). The phase has fewer columns than the block has
! lanes, so each column runs on several lanes, which share out the loops
! over it and combine their sums through shared memory; the second loop
! takes the elements of the column the first read from registers.
subroutine update(st, x, g, o)
    real, intent(inout) :: st(:, :, :)
    real, intent(in) :: x(:, :), g
    real, intent(out) :: o(:, :)
    integer :: h
    do concurrent (h = 1:size(st, 3))
        block
            real :: k(n), q(n), v(n)
            real :: sk, sq, delta
            integer :: i, j
            do concurrent (j = 1:n)
                k(j) = x(j, h)
                k(j) = k(j)*abs(k(j)) - k(j)
                q(j) = x(j, h) - 1
                q(j) = q(j) + abs(q(j))
                v(j) = 2*x(j, h) + 1
                v(j) = v(j) - abs(v(j))/2
            end do
            do concurrent (i = 1:n)
                sk = 0
                sq = 0
                do j = 1, n
                    sk = sk + st(i, j, h)*k(j)
                end do
                delta = v(i) - sk
                do j = 1, n
                    st(i, j, h) = g*st(i, j, h) + k(j)*delta
                    sq = sq + st(i, j, h)*q(j)
                end do
                o(i, h) = sq
            end do
        end block
    end do
end subroutine

! Columns of half as many lanes as the block has: each on four lanes, with
! a sum of double precision, an integer count, a maximum and a product, a
! loop whose length the lanes do not share evenly, and a loop that every
! lane of a column runs whole.
subroutine stats(a, s, c, top, p, t, r)
    real, intent(in) :: a(:, :, :)
    double precision, intent(out) :: s(:, :)
    integer, intent(out) :: c(:, :), r(:, :)
    real, intent(out) :: top(:, :), p(:, :), t(:, :)
    integer :: h
    do concurrent (h = 1:size(a, 3))
        block
            real :: w(m)
            double precision :: total
            integer :: count, i, j, rows
            real :: biggest, prod, tail
            do concurrent (j = 1:m)
                w(j) = j/16 - 2
                w(j) = w(j)*abs(w(j)) + h
            end do
            do concurrent (i = 1:m)
                total = 0
                count = 0
                biggest = -huge(1.0)
                do j = 1, 40
                    total = total + a(i, j, h)*w(j)
                    count = count + int(a(i, j, h))
                    biggest = max(biggest, a(i, j, h))
                end do
                prod = 1
                do j = 1, 30
                    prod = prod*(1 + mod(int(a(i, j, h)), 2))
                end do
                tail = 0
                rows = 0
                do j = 2, 4
                    tail = tail + a(i, j, h)
                    rows = rows + 1
                end do
                s(i, h) = total
                c(i, h) = count
                top(i, h) = biggest
                p(i, h) = prod
                t(i, h) = tail
                r(i, h) = rows
            end do
        end block
    end do
end subroutine

end module

program gpu_block_shared_04
use gpu_block_shared_04_m
implicit none
integer, parameter :: nh = 6
real :: st(n, n, nh), st0(n, n, nh), x(n, nh), o(n, nh), a(m, 40, nh)
real :: k(n), q(n), v(n), sk, sq, delta, top(m, nh), p(m, nh), t(m, nh)
double precision :: s(m, nh), total
integer :: c(m, nh), r(m, nh), h, i, j, count
real :: biggest, prod

do h = 1, nh
    do j = 1, n
        x(j, h) = mod(j + h, 5) - 2
        do i = 1, n
            st(i, j, h) = mod(i + 3*j + h, 7) - 3
        end do
    end do
end do
st0 = st
call update(st, x, 0.5, o)
do h = 1, nh
    k = x(:, h)
    k = k*abs(k) - k
    q = x(:, h) - 1
    q = q + abs(q)
    v = 2*x(:, h) + 1
    v = v - abs(v)/2
    do i = 1, n
        sk = 0
        do j = 1, n
            sk = sk + st0(i, j, h)*k(j)
        end do
        delta = v(i) - sk
        sq = 0
        do j = 1, n
            st0(i, j, h) = 0.5*st0(i, j, h) + k(j)*delta
            sq = sq + st0(i, j, h)*q(j)
        end do
        if (o(i, h) /= sq) then
            print *, "o", h, i, o(i, h), sq
            error stop 1
        end if
    end do
end do
if (any(st /= st0)) error stop 2

do h = 1, nh
    do j = 1, 40
        do i = 1, m
            a(i, j, h) = mod(i*j + h, 9) - 4
        end do
    end do
end do
call stats(a, s, c, top, p, t, r)
do h = 1, nh
    do i = 1, m
        total = 0
        count = 0
        biggest = -huge(biggest)
        do j = 1, 40
            total = total + a(i, j, h)*((j/16 - 2)*abs(j/16 - 2) + h)
            count = count + int(a(i, j, h))
            biggest = max(biggest, a(i, j, h))
        end do
        prod = 1
        do j = 1, 30
            prod = prod*(1 + mod(int(a(i, j, h)), 2))
        end do
        if (s(i, h) /= total .or. c(i, h) /= count .or. top(i, h) /= biggest &
                .or. p(i, h) /= prod .or. t(i, h) /= sum(a(i, 2:4, h)) &
                .or. r(i, h) /= 3) then
            print *, "stats", h, i, s(i, h), total, c(i, h), count, &
                top(i, h), biggest, p(i, h), prod, t(i, h), r(i, h)
            error stop 3
        end if
    end do
end do
print *, "ok"
end program
