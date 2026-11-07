module gpu_lanes_columns_01_m
! Launches of many more iterations than the 1024 a launch usually gives the
! lanes of a SIMD-group to (gpu_vector_lanes.cpp): when every loop the lanes
! share out walks a column of an array that the iteration takes a column of
! -- the rows of a matrix-vector product stored as columns, the groups of
! a quantized one -- one lane per iteration would read 32 columns at once,
! and the lanes read one together, so they get lanes at any size. A kernel
! that repeats a column walk on every lane, as a row norm whose sum is a
! call, keeps one lane per iteration. Every result is compared exactly.
implicit none
contains
    pure real function sum_squares(x) result(total)
        real, intent(in) :: x(:)
        total = sum(x**2)
    end function

    pure subroutine products(w, x, y)
        real, contiguous, intent(in) :: w(:, :), x(:)
        real, contiguous, intent(out) :: y(:)
        integer :: n
        do concurrent (n = 1:size(w, 2))
            block
                real :: acc
                integer :: d
                acc = 0
                do d = 1, size(w, 1)
                    acc = acc + w(d, n)*x(d)
                end do
                y(n) = acc
            end block
        end do
    end subroutine

    pure subroutine grouped(w, x, s, y)
        real, contiguous, intent(in) :: w(:, :), x(:), s(:, :)
        real, contiguous, intent(out) :: y(:)
        integer :: groups
        integer :: n
        groups = size(w, 1)/16
        do concurrent (n = 1:size(w, 2))
            block
                real :: terms(groups)
                integer :: g
                do g = 1, groups
                    block
                        real :: acc
                        integer :: d
                        acc = 0
                        do d = (g - 1)*16 + 1, g*16
                            acc = acc + w(d, n)*x(d)
                        end do
                        terms(g) = s(g, n)*acc
                    end block
                end do
                y(n) = sum(terms)
            end block
        end do
    end subroutine

    pure subroutine norms(x, y)
        real, contiguous, intent(in) :: x(:, :)
        real, contiguous, intent(out) :: y(:, :)
        integer :: m
        do concurrent (m = 1:size(x, 2))
            block
                real :: scale
                integer :: d
                scale = sum_squares(x(:, m))
                do d = 1, size(x, 1)
                    y(d, m) = x(d, m)*scale
                end do
            end block
        end do
    end subroutine
end module

program gpu_lanes_columns_01
use gpu_lanes_columns_01_m
implicit none
integer, parameter :: k = 256, n = 5000
real :: w(k, n), x(k), y(n), z(n), s(k/16, n), a(64, 2000), b(64, 2000)
real :: ref
integer :: i, j, g
do j = 1, n
    do i = 1, k
        w(i, j) = real(mod(i + 7*j, 9)) - 4
    end do
    do g = 1, k/16
        s(g, j) = real(mod(g + j, 3))
    end do
end do
do i = 1, k
    x(i) = real(mod(3*i, 5))
end do
do j = 1, 2000
    do i = 1, 64
        a(i, j) = real(mod(i*j, 7)) - 3
    end do
end do
call products(w, x, y)
call grouped(w, x, s, z)
call norms(a, b)
do j = 1, n
    if (y(j) /= sum(w(:, j)*x)) error stop "products"
    ref = 0
    do g = 1, k/16
        ref = ref + s(g, j)*sum(w((g - 1)*16 + 1:g*16, j)*x((g - 1)*16 + 1:g*16))
    end do
    if (z(j) /= ref) error stop "grouped"
end do
do j = 1, 2000
    if (any(b(:, j) /= a(:, j)*sum(a(:, j)**2))) error stop "norms"
end do
print *, "ok"
end program
