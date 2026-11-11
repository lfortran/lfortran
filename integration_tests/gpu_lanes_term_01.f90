module gpu_lanes_term_01_m
! The matrix-vector product of quantized weights, where each group adds a
! factor times the sum of its codes and a term (offset times the sum of the
! vector over the group) that depends on the group alone. The lanes that
! share out the codes of a group add the terms of the groups of their own
! after the loop over the groups, rather than the first lane adding each in
! turn. Integer-valued data keeps the result independent of the order of the
! sums; a launch of many rows runs a row on one thread.
implicit none
contains
    pure function code(word, part) result(value)
        integer, intent(in) :: word, part
        integer :: value
        value = ibits(word, 8*part, 8)
    end function

    pure subroutine qmv(w, scales, offsets, x, sums, y)
        integer, contiguous, intent(in) :: w(:, :)
        real, contiguous, intent(in) :: scales(:, :), offsets(:, :), x(:), sums(:)
        real, contiguous, intent(out) :: y(:)
        integer :: groups, n

        groups = size(x)/64
        do concurrent (n = 1:size(w, 2))
            block
                integer :: g, d
                real :: acc, total, term

                total = 0
                do g = 1, groups
                    acc = 0
                    do d = (g - 1)*64 + 1, g*64
                        acc = acc + real(code(w((d - 1)/4 + 1, n), modulo(d - 1, 4)))*x(d)
                    end do
                    term = scales(g, n)*acc - offsets(g, n)*sums(g)
                    total = total + term
                end do
                y(n) = total
            end block
        end do
    end subroutine
end module

program gpu_lanes_term_01
use gpu_lanes_term_01_m
implicit none
integer, parameter :: k = 320, groups = k/64
integer, parameter :: sizes(3) = [5, 37, 70001]
integer, allocatable :: w(:, :)
real, allocatable :: scales(:, :), offsets(:, :), y(:)
real :: x(k), sums(groups), ref
integer :: s, nrows, i, j, g, d, c

do i = 1, k
    x(i) = mod(3*i, 5) - 2
end do
do g = 1, groups
    sums(g) = sum(x((g - 1)*64 + 1:g*64))
end do
do s = 1, size(sizes)
    nrows = sizes(s)
    allocate(w(k/4, nrows), scales(groups, nrows), offsets(groups, nrows), y(nrows))
    do j = 1, nrows
        do i = 1, k/4
            w(i, j) = ior(ior(mod(i + j, 251), ishft(mod(3*i + j, 253), 8)), &
                ior(ishft(mod(5*i + 2*j, 249), 16), ishft(mod(7*i + j, 255), 24)))
        end do
        do g = 1, groups
            scales(g, j) = 1 + mod(g + j, 3)
            offsets(g, j) = mod(2*g + j, 5) - 2
        end do
    end do
    call qmv(w, scales, offsets, x, sums, y)
    do j = 1, nrows
        ref = 0
        do g = 1, groups
            c = 0
            do d = (g - 1)*64 + 1, g*64
                c = c + ibits(w((d - 1)/4 + 1, j), 8*modulo(d - 1, 4), 8)*int(x(d))
            end do
            ref = ref + (scales(g, j)*c - offsets(g, j)*sums(g))
        end do
        if (y(j) /= ref) error stop "quantized matrix-vector product"
    end do
    deallocate(w, scales, offsets, y)
end do
print *, "ok"
end program
