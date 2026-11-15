module gpu_launch_bounds_m
! A matrix-vector product of packed 8-bit words whose lanes read vectors of 16
! bytes. On an NVIDIA GPU its kernel is bound to the registers of six blocks
! of 256 threads per multiprocessor and does not unroll the loop of its lanes.
! The data is integer-valued, so that the sums are exact.
implicit none
contains
    pure function dot4(a, b, c) result(r)
        integer, intent(in) :: a, b, c
        integer :: r
        r = c + ibits(a, 0, 8)*ibits(b, 0, 8) + ibits(a, 8, 8)*ibits(b, 8, 8) &
            + ibits(a, 16, 8)*ibits(b, 16, 8) + ibits(a, 24, 8)*ibits(b, 24, 8)
    end function

    pure function chunk_idot(w, xw, n, c) result(r)
        integer, intent(in) :: w(:, :), xw(:)
        integer, intent(in) :: n, c
        integer :: r
        r = dot4(w(4*c-3, n), xw(4*c-3), dot4(w(4*c-2, n), xw(4*c-2), &
            dot4(w(4*c-1, n), xw(4*c-1), dot4(w(4*c, n), xw(4*c), 0))))
    end function

    pure subroutine qmv(xw, sx, w, y)
        integer, contiguous, intent(in) :: xw(:), w(:, :)
        real, contiguous, intent(in) :: sx(:)
        real, contiguous, intent(out) :: y(:)
        integer :: k, n

        k = 4*size(w, 1)
        do concurrent (n = 1:size(y))
            block
                real :: total
                integer :: q, c

                total = 0
                do q = 1, k/32
                    c = 2*q - 1
                    total = total + sx(c)*real(chunk_idot(w, xw, n, c))
                    total = total + sx(c+1)*real(chunk_idot(w, xw, n, c+1))
                end do
                y(n) = total
            end block
        end do
    end subroutine
end module

program gpu_launch_bounds_01
use gpu_launch_bounds_m
implicit none
integer, parameter :: rows = 4099, words = 1280
integer, allocatable :: w(:, :), xw(:)
real, allocatable :: sx(:), y(:)
real :: expected
integer :: n, i, c, j, k, d

allocate(w(words, rows), xw(words), sx(words/4), y(rows))
do n = 1, rows
    do i = 1, words
        w(i, n) = ior(ior(mod(i + n, 4), ishft(mod(i + 2*n, 3), 8)), &
            ior(ishft(mod(i, 5), 16), ishft(mod(n, 2), 24)))
    end do
end do
do i = 1, words
    xw(i) = ior(ior(mod(i, 3), ishft(mod(i, 2), 8)), ior(ishft(1, 16), ishft(mod(i, 4), 24)))
end do
do c = 1, words/4
    sx(c) = real(mod(c, 3) + 1)
end do
call qmv(xw, sx, w, y)
do n = 1, rows
    expected = 0
    do c = 1, words/4
        k = 0
        do j = 4*c - 3, 4*c
            do d = 0, 24, 8
                k = k + ibits(w(j, n), d, 8)*ibits(xw(j), d, 8)
            end do
        end do
        expected = expected + sx(c)*real(k)
    end do
    if (y(n) /= expected) then
        print *, "row", n, y(n), expected
        error stop
    end if
end do
print *, "ok"
end program
