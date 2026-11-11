module gpu_lanes_defer_01_m
! The sum over the groups of a quantized matrix-vector product, where each
! group adds a factor times the sum of its 64 codes, and a term that does not
! depend on it. The sum of a group is linear in the sum of its codes, so the
! lanes that share out the codes of a group need not combine their sums in
! every group: each lane scales what it holds, and they combine the total
! once. The first lane alone adds the term of the group. Integer-valued data
! keeps the result independent of the order of the sums.
implicit none
contains
    pure function code(word, part) result(value)
        integer, intent(in) :: word, part
        integer :: value
        value = ibits(word, 8*part, 8)
    end function

    ! total = total + scale*sum + offset
    pure subroutine qmv_sum(w, scales, offsets, x, sums, y)
        integer, contiguous, intent(in) :: w(:, :)
        real, contiguous, intent(in) :: scales(:, :), offsets(:, :), x(:, :), sums(:, :)
        real, contiguous, intent(out) :: y(:, :)
        integer :: groups, n

        groups = size(x, 1)/64
        do concurrent (n = 1:size(w, 2))
            block
                integer :: m, g, d
                real :: acc, total

                do m = 1, size(x, 2)
                    total = 0
                    do g = 1, groups
                        acc = 0
                        do d = (g - 1)*64 + 1, g*64
                            acc = acc + real(code(w((d - 1)/4 + 1, n), modulo(d - 1, 4)))*x(d, m)
                        end do
                        total = total + (scales(g, n)*acc + offsets(g, n)*sums(g, m))
                    end do
                    y(n, m) = total
                end do
            end block
        end do
    end subroutine

    ! total = total + (acc - offset)/scale, with the sum in a scalar of its own
    pure subroutine qmv_quotient(w, scales, offsets, x, y)
        integer, contiguous, intent(in) :: w(:, :)
        real, contiguous, intent(in) :: scales(:, :), offsets(:, :), x(:, :)
        real, contiguous, intent(out) :: y(:, :)
        integer :: groups, n

        groups = size(x, 1)/64
        do concurrent (n = 1:size(w, 2))
            block
                integer :: m, g, d
                real :: acc, total, term

                do m = 1, size(x, 2)
                    total = 0
                    do g = 1, groups
                        acc = 0
                        do d = (g - 1)*64 + 1, g*64
                            acc = acc + real(code(w((d - 1)/4 + 1, n), modulo(d - 1, 4)))*x(d, m)
                        end do
                        term = (acc - offsets(g, n))/scales(g, n)
                        total = total + term
                    end do
                    y(n, m) = total
                end do
            end block
        end do
    end subroutine

    ! The sum of a group is rounded before it is added: not linear, so the
    ! lanes combine it in every group.
    pure subroutine qmv_clamped(w, scales, x, y)
        integer, contiguous, intent(in) :: w(:, :)
        real, contiguous, intent(in) :: scales(:, :), x(:, :)
        real, contiguous, intent(out) :: y(:, :)
        integer :: groups, n

        groups = size(x, 1)/64
        do concurrent (n = 1:size(w, 2))
            block
                integer :: m, g, d
                real :: acc, total

                do m = 1, size(x, 2)
                    total = 0
                    do g = 1, groups
                        acc = 0
                        do d = (g - 1)*64 + 1, g*64
                            acc = acc + real(code(w((d - 1)/4 + 1, n), modulo(d - 1, 4)))*x(d, m)
                        end do
                        total = total + max(scales(g, n)*acc, 0.0)
                    end do
                    y(n, m) = total
                end do
            end block
        end do
    end subroutine
end module

program gpu_lanes_defer_01
use gpu_lanes_defer_01_m
implicit none
integer, parameter :: k = 320, groups = k/64, nrows = 37, nm = 2
integer :: w(k/4, nrows), i, j, m, g, d, c
real :: scales(groups, nrows), offsets(groups, nrows), x(k, nm), sums(groups, nm)
real :: y(nrows, nm), ref
do j = 1, nrows
    do i = 1, k/4
        w(i, j) = ior(ior(mod(i + j, 251), ishft(mod(3*i + j, 253), 8)), &
            ior(ishft(mod(5*i + 2*j, 249), 16), ishft(mod(7*i + j, 255), 24)))
    end do
    do g = 1, groups
        scales(g, j) = 1 + mod(g + j, 2)*1
        offsets(g, j) = mod(2*g + j, 5) - 2
    end do
end do
do m = 1, nm
    do i = 1, k
        x(i, m) = mod(3*i + m, 5) - 2
    end do
    do g = 1, groups
        sums(g, m) = sum(x((g - 1)*64 + 1:g*64, m))
    end do
end do

call qmv_sum(w, scales, offsets, x, sums, y)
do m = 1, nm
    do j = 1, nrows
        ref = 0
        do g = 1, groups
            c = 0
            do d = (g - 1)*64 + 1, g*64
                c = c + ibits(w((d - 1)/4 + 1, j), 8*modulo(d - 1, 4), 8)*int(x(d, m))
            end do
            ref = ref + scales(g, j)*c + offsets(g, j)*sums(g, m)
        end do
        if (y(j, m) /= ref) error stop "sum of scaled groups and terms"
    end do
end do

! Powers of two as scales divide exactly.
do j = 1, nrows
    do g = 1, groups
        scales(g, j) = 2.0**mod(g + j, 2)
    end do
end do
call qmv_quotient(w, scales, offsets, x, y)
do m = 1, nm
    do j = 1, nrows
        ref = 0
        do g = 1, groups
            c = 0
            do d = (g - 1)*64 + 1, g*64
                c = c + ibits(w((d - 1)/4 + 1, j), 8*modulo(d - 1, 4), 8)*int(x(d, m))
            end do
            ref = ref + (real(c) - offsets(g, j))/scales(g, j)
        end do
        if (y(j, m) /= ref) error stop "quotient"
    end do
end do

call qmv_clamped(w, scales, x, y)
do m = 1, nm
    do j = 1, nrows
        ref = 0
        do g = 1, groups
            c = 0
            do d = (g - 1)*64 + 1, g*64
                c = c + ibits(w((d - 1)/4 + 1, j), 8*modulo(d - 1, 4), 8)*int(x(d, m))
            end do
            ref = ref + max(scales(g, j)*real(c), 0.0)
        end do
        if (y(j, m) /= ref) error stop "clamped"
    end do
end do
print *, "ok"
end program
