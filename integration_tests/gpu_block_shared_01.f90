module gpu_block_shared_01_m
implicit none
integer, parameter :: n = 64
contains

! Each iteration fills three arrays of its own with an inner `do concurrent`,
! sums over them, reads all of two of them in a phase over the columns of a
! matrix, sums the result of that phase and scales it in a last phase. On an
! NVIDIA GPU the iteration runs on the lanes of a whole block, and its arrays
! live in the shared memory of the block (gpu_vector_lanes.cpp): the lanes
! share out the loops that assign them and meet at a barrier before the
! sums and the phases that read them.
subroutine step(x, w, y, s1, s2)
    real, intent(in) :: x(:, :), w(:, :, :)
    real, intent(out) :: y(:, :), s1(:), s2(:)
    integer :: h
    do concurrent (h = 1:size(x, 2))
        block
            real :: a(n), b(n), c(n), o(n)
            real :: sa, sb, so, t
            integer :: i, j
            do concurrent (j = 1:n)
                a(j) = x(j, h) + 1
                a(j) = a(j)*abs(a(j))
                b(j) = 2*x(j, h)
                b(j) = b(j) - abs(b(j))/2
                c(j) = x(j, h) - j
                c(j) = c(j) + abs(c(j))
            end do
            sa = 0
            sb = 0
            do j = 1, n
                sa = sa + a(j)
                sb = sb + b(j)*c(j)
            end do
            do concurrent (i = 1:n)
                t = 0
                do j = 1, n
                    t = t + w(i, j, h)*a(j)
                end do
                o(i) = t + b(i)*sa
            end do
            so = 0
            do j = 1, n
                so = so + o(j)
            end do
            do concurrent (i = 1:n)
                y(i, h) = 2*o(i) - so + c(i)
            end do
            s1(h) = sa
            s2(h) = sb
        end block
    end do
end subroutine

end module

program gpu_block_shared_01
! Few iterations, which get the lanes of a block anyway, and many, which
! still run one iteration per block: the arrays of an iteration are the
! block's. The values are small integers, so every sum is exact.
use gpu_block_shared_01_m
implicit none
integer :: sizes(3), k, nh, h, i, j
real, allocatable :: x(:, :), w(:, :, :), y(:, :), s1(:), s2(:)
real :: a(n), b(n), c(n), o(n), sa, sb, so, t

sizes = [1, 5, 3000]
do k = 1, size(sizes)
    nh = sizes(k)
    allocate(x(n, nh), w(n, n, nh), y(n, nh), s1(nh), s2(nh))
    do h = 1, nh
        do j = 1, n
            x(j, h) = mod(3*j + 5*h, 7) - 3
            do i = 1, n
                w(i, j, h) = mod(i + 2*j + h, 5) - 2
            end do
        end do
    end do
    call step(x, w, y, s1, s2)
    do h = 1, nh
        do j = 1, n
            a(j) = x(j, h) + 1
            a(j) = a(j)*abs(a(j))
            b(j) = 2*x(j, h)
            b(j) = b(j) - abs(b(j))/2
            c(j) = x(j, h) - j
            c(j) = c(j) + abs(c(j))
        end do
        sa = sum(a)
        sb = sum(b*c)
        do i = 1, n
            t = 0
            do j = 1, n
                t = t + w(i, j, h)*a(j)
            end do
            o(i) = t + b(i)*sa
        end do
        so = sum(o)
        if (s1(h) /= sa .or. s2(h) /= sb) then
            print *, "sums", nh, h, s1(h), sa, s2(h), sb
            error stop 1
        end if
        do i = 1, n
            if (y(i, h) /= 2*o(i) - so + c(i)) then
                print *, "y", nh, h, i, y(i, h), 2*o(i) - so + c(i)
                error stop 2
            end if
        end do
    end do
    deallocate(x, w, y, s1, s2)
end do
print *, "ok"
end program
