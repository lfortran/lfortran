module gpu_lanes_parts_01_m
! A sum split in a few parts, each a sum along the column of the iteration
! (the partial sums of a matrix-vector product reduced in a fixed order):
! the lanes of a SIMD-group share out each part's sum, rather than the
! parts, which would leave most of them idle (gpu_vector_lanes.cpp). Every
! result is compared exactly.
implicit none
contains
    pure subroutine split(w, x, parts, y)
        real, contiguous, intent(in) :: w(:, :), x(:)
        integer, intent(in) :: parts
        real, contiguous, intent(out) :: y(:)
        integer :: n
        do concurrent (n = 1:size(w, 2))
            block
                real :: partial(parts), acc
                integer :: p, d, length
                length = size(w, 1)/parts
                do p = 1, parts
                    acc = 0
                    do d = (p - 1)*length + 1, p*length
                        acc = acc + w(d, n)*x(d)
                    end do
                    partial(p) = acc
                end do
                y(n) = sum(partial)
            end block
        end do
    end subroutine
end module

program gpu_lanes_parts_01
use gpu_lanes_parts_01_m
implicit none
integer, parameter :: k = 480, n = 3000
real :: w(k, n), x(k), y(n)
integer :: i, j, parts
do j = 1, n
    do i = 1, k
        w(i, j) = real(mod(i + 11*j, 9)) - 4
    end do
end do
do i = 1, k
    x(i) = real(mod(7*i, 5)) - 2
end do
do parts = 1, 3
    call split(w, x, parts, y)
    do j = 1, n
        if (y(j) /= sum(w(:, j)*x)) error stop "split"
    end do
end do
print *, "ok"
end program
