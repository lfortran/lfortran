module gpu_private_reduction_02_m
! A private map that only `sum` reads, right after the concurrent loop that
! assigns it by the last statement of each iteration -- the group terms of
! a quantized matrix-vector product, each the scaled sum of a group --
! needs no storage: the iterations add their terms to the sum instead
! (gpu_private_reduction_fusion), here in a branch of an `if`, for real and
! integer terms. A map that is read again keeps its storage. Every result
! is compared exactly.
implicit none
contains
    pure subroutine grouped(w, s, x, y, scaled)
        real, contiguous, intent(in) :: w(:, :), s(:, :), x(:)
        real, contiguous, intent(out) :: y(:)
        logical, intent(in) :: scaled
        integer :: groups
        integer :: n, g
        groups = size(w, 1)/32
        do concurrent (n = 1:size(w, 2))
            block
                real :: terms(groups), total
                if (scaled) then
                    do concurrent (g = 1:groups)
                        terms(g) = s(g, n)*sum(w((g - 1)*32 + 1:g*32, n)*x((g - 1)*32 + 1:g*32))
                    end do
                    total = sum(terms)
                else
                    total = sum(w(:, n)*x)
                end if
                y(n) = total
            end block
        end do
    end subroutine

    pure subroutine counts(q, y)
        integer, contiguous, intent(in) :: q(:, :)
        integer, contiguous, intent(out) :: y(:)
        integer :: groups
        integer :: n, g
        groups = size(q, 1)/8
        do concurrent (n = 1:size(q, 2))
            block
                integer :: terms(groups)
                do concurrent (g = 1:groups)
                    terms(g) = count(q((g - 1)*8 + 1:g*8, n) > 2)*g
                end do
                y(n) = sum(terms)
            end block
        end do
    end subroutine

    pure subroutine reread(w, x, y, z)
        real, contiguous, intent(in) :: w(:, :), x(:)
        real, contiguous, intent(out) :: y(:), z(:)
        integer :: groups
        integer :: n, g
        groups = size(w, 1)/32
        do concurrent (n = 1:size(w, 2))
            block
                real :: terms(groups)
                do concurrent (g = 1:groups)
                    terms(g) = sum(w((g - 1)*32 + 1:g*32, n)*x((g - 1)*32 + 1:g*32))
                end do
                y(n) = sum(terms)
                z(n) = terms(1)
            end block
        end do
    end subroutine
end module

program gpu_private_reduction_02
use gpu_private_reduction_02_m
implicit none
integer, parameter :: k = 256, n = 2000
real :: w(k, n), s(k/32, n), x(k), y(n), z(n), ref
integer :: q(k, n), c(n)
integer :: i, j, g, iref
do j = 1, n
    do i = 1, k
        w(i, j) = real(mod(i + 3*j, 7)) - 3
        q(i, j) = mod(i*j, 5)
    end do
    do g = 1, k/32
        s(g, j) = real(mod(g + j, 4))
    end do
end do
do i = 1, k
    x(i) = real(mod(5*i, 3))
end do
call grouped(w, s, x, y, .true.)
do j = 1, n
    ref = 0
    do g = 1, k/32
        ref = ref + s(g, j)*sum(w((g - 1)*32 + 1:g*32, j)*x((g - 1)*32 + 1:g*32))
    end do
    if (y(j) /= ref) error stop "grouped"
end do
call grouped(w, s, x, y, .false.)
do j = 1, n
    if (y(j) /= sum(w(:, j)*x)) error stop "plain"
end do
call counts(q, c)
do j = 1, n
    iref = 0
    do g = 1, k/8
        iref = iref + count(q((g - 1)*8 + 1:g*8, j) > 2)*g
    end do
    if (c(j) /= iref) error stop "counts"
end do
call reread(w, x, y, z)
do j = 1, n
    if (y(j) /= sum(w(:, j)*x)) error stop "reread sum"
    if (z(j) /= sum(w(1:32, j)*x(1:32))) error stop "reread first"
end do
print *, "ok"
end program
