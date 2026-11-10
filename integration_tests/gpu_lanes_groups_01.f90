module gpu_lanes_groups_01_m
! The matrix-vector product of quantized weights: four 8-bit codes packed in
! each 32-bit word, a scale and an offset per group of 64 codes, and
! `do concurrent` over the rows. On an NVIDIA GPU, a launch of few iterations
! runs each on the lanes of a warp, and the lanes share out the sum over the
! 64 codes of a group (16 words) rather than the groups: they then read the
! words and the vector along consecutive elements. Such a sum is shared out
! by 16 lanes, so two iterations run on a warp, and an odd number of rows
! leaves the second half of the last warp without an iteration.
implicit none
contains
    pure function code(word, part) result(value)
        integer, intent(in) :: word, part
        integer :: value
        value = ibits(word, 8*part, 8)
    end function

    pure subroutine qmv(w, scales, offsets, x, sums, y)
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
                        total = total + scales(g, n)*acc + offsets(g, n)*sums(g, m)
                    end do
                    y(n, m) = total
                end do
            end block
        end do
    end subroutine
end module

program gpu_lanes_groups_01
use gpu_lanes_groups_01_m
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
        scales(g, j) = 1 + mod(g + j, 3)
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
call qmv(w, scales, offsets, x, sums, y)
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
        if (y(j, m) /= ref) error stop "quantized matrix-vector product"
    end do
end do
print *, "ok"
end program
