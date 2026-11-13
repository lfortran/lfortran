module gpu_wide_loads_01_m
! The matrix-vector product of quantized weights, where a lane takes the 16
! codes of four packed words and the 16 activations they multiply, in one
! statement. On an NVIDIA GPU these are read as vectors of 16 bytes when the
! arrays start at a multiple of 16 bytes, and one by one when they do not (the
! activations passed from the second element of an array). Integer-valued
! data keeps the result independent of the order of the sums.
implicit none
contains
    pure function chunk_dot(w, x, n, c) result(r)
        integer, intent(in) :: w(:, :)
        integer(2), intent(in) :: x(:)
        integer, intent(in) :: n, c
        real :: r

        r = real(ibits(w(4*c - 3, n), 0, 8))*real(x(16*c - 15)) &
          + real(ibits(w(4*c - 3, n), 8, 8))*real(x(16*c - 14)) &
          + real(ibits(w(4*c - 3, n), 16, 8))*real(x(16*c - 13)) &
          + real(ibits(w(4*c - 3, n), 24, 8))*real(x(16*c - 12)) &
          + real(ibits(w(4*c - 2, n), 0, 8))*real(x(16*c - 11)) &
          + real(ibits(w(4*c - 2, n), 8, 8))*real(x(16*c - 10)) &
          + real(ibits(w(4*c - 2, n), 16, 8))*real(x(16*c - 9)) &
          + real(ibits(w(4*c - 2, n), 24, 8))*real(x(16*c - 8)) &
          + real(ibits(w(4*c - 1, n), 0, 8))*real(x(16*c - 7)) &
          + real(ibits(w(4*c - 1, n), 8, 8))*real(x(16*c - 6)) &
          + real(ibits(w(4*c - 1, n), 16, 8))*real(x(16*c - 5)) &
          + real(ibits(w(4*c - 1, n), 24, 8))*real(x(16*c - 4)) &
          + real(ibits(w(4*c, n), 0, 8))*real(x(16*c - 3)) &
          + real(ibits(w(4*c, n), 8, 8))*real(x(16*c - 2)) &
          + real(ibits(w(4*c, n), 16, 8))*real(x(16*c - 1)) &
          + real(ibits(w(4*c, n), 24, 8))*real(x(16*c))
    end function

    pure subroutine qmv(w, x, y)
        integer, contiguous, intent(in) :: w(:, :)
        integer(2), contiguous, intent(in) :: x(:)
        real, contiguous, intent(out) :: y(:)
        integer :: n

        do concurrent (n = 1:size(w, 2))
            block
                integer :: c
                real :: total

                total = 0
                do c = 1, size(x)/16
                    total = total + chunk_dot(w, x, n, c)
                end do
                y(n) = total
            end block
        end do
    end subroutine
end module

program gpu_wide_loads_01
use gpu_wide_loads_01_m
implicit none
integer, parameter :: k = 320
integer, parameter :: sizes(3) = [5, 37, 70001]
integer, allocatable :: w(:, :)
integer(2), allocatable :: xs(:)
integer(2) :: x(k + 1)
real, allocatable :: y(:)
real :: ref
integer :: s, nrows, i, j, shift, c

do shift = 0, 1
    ! shift = 1 passes the activations from their second element: 2 bytes
    ! past the multiple of 16 the array starts at
    allocate(xs(k + 1))
    do i = 1, k + 1
        x(i) = int(mod(3*i, 5) - 2, 2)
    end do
    xs = x
    do s = 1, size(sizes)
        nrows = sizes(s)
        allocate(w(k/4, nrows), y(nrows))
        do j = 1, nrows
            do i = 1, k/4
                w(i, j) = ior(ior(mod(i + j, 251), ishft(mod(3*i + j, 253), 8)), &
                    ior(ishft(mod(5*i + 2*j, 249), 16), ishft(mod(7*i + j, 255), 24)))
            end do
        end do
        call qmv(w, xs(1 + shift:k + shift), y)
        do j = 1, nrows
            ref = 0
            do i = 1, k
                c = (i - 1)/4 + 1
                ref = ref + ibits(w(c, j), 8*modulo(i - 1, 4), 8)*int(xs(i + shift))
            end do
            if (y(j) /= ref) error stop "quantized matrix-vector product"
        end do
        deallocate(w, y)
    end do
    deallocate(xs)
end do
print *, "ok"
end program
