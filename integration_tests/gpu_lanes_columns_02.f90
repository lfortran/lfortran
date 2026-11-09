module gpu_lanes_columns_02_m
! Sums along the columns of launches of more than 1024 iterations, which
! the lanes of a SIMD-group share out and combine (gpu_vector_lanes.cpp)
! although the loop also steps an index of its own into another array, as
! the second index of an array statement whose sections start at different
! subscripts does, the loop has blocks inside it, its body adds to the sum
! more than once, or what the loop leaves in its variable or in that index
! is read after it. Every result is compared exactly.
use, intrinsic :: iso_fortran_env, only: int16, int32
implicit none
contains
    ! The shape of a quantized matrix-vector product: groups of codes
    ! packed four to a word, unpacked by a map that each read recomputes,
    ! and summed group by group in one branch, at once in the other.
    pure subroutine packed(w, s, x, y, grouped)
        integer(int32), contiguous, intent(in) :: w(:, :)
        real, contiguous, intent(in) :: s(:, :), x(:, :)
        real, contiguous, intent(out) :: y(:, :)
        logical, intent(in) :: grouped
        integer :: k, groups
        integer :: n, d, m, g
        k = 4*size(w, 1)
        groups = k/64
        do concurrent (n = 1:size(y, 1))
            block
                integer(int16) :: codes(k)
                real :: weights(k)
                do concurrent (d = 1:k)
                    codes(d) = int(ibits(w((d - 1)/4 + 1, n), 8*modulo(d - 1, 4), 8), int16)
                    if (.not. grouped) weights(d) = real(codes(d))*s((d - 1)/64 + 1, n)
                end do
                do concurrent (m = 1:size(x, 2))
                    block
                        real :: terms(groups), total
                        if (grouped) then
                            do concurrent (g = 1:groups)
                                terms(g) = s(g, n)*sum(real(codes((g - 1)*64 + 1:g*64)) &
                                    *x((g - 1)*64 + 1:g*64, m))
                            end do
                            total = sum(terms)
                        else
                            total = sum(weights*x(:, m))
                        end if
                        y(n, m) = total
                    end block
                end do
            end block
        end do
    end subroutine

    ! An index `j` of its own and two additions per iteration, with the
    ! variable and the index read after the loop, which may run no
    ! iteration at all.
    pure subroutine strided(a, x, first, y)
        real, contiguous, intent(in) :: a(:, :), x(:)
        integer, intent(in) :: first
        real, contiguous, intent(out) :: y(:)
        integer :: n
        do concurrent (n = 1:size(a, 2))
            block
                real :: acc
                integer :: i, j
                acc = 0
                j = first + 1
                do i = first, size(a, 1) - 1, 2
                    acc = acc + a(i, n)*x(j)
                    acc = acc + a(i + 1, n)*x(j + 1)
                    j = j + 2
                end do
                y(n) = acc + 1000*i + 100000*j
            end block
        end do
    end subroutine
end module

program gpu_lanes_columns_02
use gpu_lanes_columns_02_m
implicit none
integer, parameter :: k = 512, n = 3000, tokens = 2, rows = 129
integer(int32) :: w(k/4, n)
real :: s(k/64, n), x(k, tokens), y(n, tokens), z(n, tokens), ref
real :: a(rows, n), v(rows + 1), p(n)
integer :: i, j, t, b, first, last
do j = 1, n
    do i = 1, k/4
        w(i, j) = 0
        do b = 0, 3
            w(i, j) = ior(w(i, j), ishft(mod(i + 3*j + 5*b, 7), 8*b))
        end do
    end do
    do i = 1, k/64
        s(i, j) = real(mod(i + j, 3) + 1)
    end do
    do i = 1, rows
        a(i, j) = real(mod(i + 5*j, 7)) - 3
    end do
end do
do t = 1, tokens
    do i = 1, k
        x(i, t) = real(mod(3*i + t, 5)) - 2
    end do
end do
do i = 1, rows + 1
    v(i) = real(mod(2*i, 3))
end do
call packed(w, s, x, y, .true.)
call packed(w, s, x, z, .false.)
do t = 1, tokens
    do j = 1, n
        ref = 0
        do i = 1, k
            ref = ref + real(ibits(w((i - 1)/4 + 1, j), 8*modulo(i - 1, 4), 8)) &
                *s((i - 1)/64 + 1, j)*x(i, t)
        end do
        if (y(j, t) /= ref) error stop "grouped"
        if (z(j, t) /= ref) error stop "packed"
    end do
end do
do first = 1, rows + 2, 64
    call strided(a, v, first, p)
    last = first + 2*max(0, (rows - 1 - first + 2)/2)
    do j = 1, n
        ref = 0
        do i = first, rows - 1, 2
            ref = ref + a(i, j)*v(i + 1) + a(i + 1, j)*v(i + 2)
        end do
        ref = ref + 1000*last + 100000*(last + 1)
        if (p(j) /= ref) error stop "strided"
    end do
end do
print *, "ok"
end program
