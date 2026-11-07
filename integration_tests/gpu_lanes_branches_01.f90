module gpu_lanes_branches_01_m
! A loop inside one branch of an `if`, inside a loop of a `do concurrent`
! iteration, as a quantized matrix-vector product chooses between its
! vector and matrix paths: the lanes of a SIMD-group share it out
! (gpu_vector_lanes.cpp) when the scalars it assigns are dead after it,
! which takes looking at the other branch, on the way to the next
! iteration of the loop around the `if`. Every result is compared exactly.
implicit none
contains
    pure subroutine grouped(w, x, s, y, vector)
        real, contiguous, intent(in) :: w(:, :), x(:, :), s(:, :)
        real, contiguous, intent(out) :: y(:, :)
        logical, intent(in) :: vector
        integer :: groups
        integer :: n
        groups = size(w, 1)/8
        do concurrent (n = 1:size(w, 2))
            block
                real :: terms(groups), total, acc
                integer :: m, g, d
                do m = 1, size(x, 2)
                    if (vector) then
                        do g = 1, groups
                            acc = 0
                            do d = (g - 1)*8 + 1, g*8
                                acc = acc + w(d, n)*x(d, m)
                            end do
                            terms(g) = s(g, n)*acc
                        end do
                        total = sum(terms)
                    else
                        acc = 0
                        do d = 1, size(w, 1)
                            acc = acc + w(d, n)*x(d, m)*s((d - 1)/8 + 1, n)
                        end do
                        total = acc
                    end if
                    y(n, m) = total
                end do
            end block
        end do
    end subroutine
end module

program gpu_lanes_branches_01
use gpu_lanes_branches_01_m
implicit none
integer, parameter :: k = 512, nout = 40, nm = 2
real :: w(k, nout), x(k, nm), s(k/8, nout), y(nout, nm), z(nout, nm), ref
integer :: i, j, m, g
do j = 1, nout
    do i = 1, k
        w(i, j) = real(mod(i + 3*j, 7)) - 3
    end do
    do g = 1, k/8
        s(g, j) = real(mod(g + j, 3))
    end do
end do
do m = 1, nm
    do i = 1, k
        x(i, m) = real(mod(5*i + m, 4))
    end do
end do
call grouped(w, x, s, y, .true.)
call grouped(w, x, s, z, .false.)
do m = 1, nm
    do j = 1, nout
        ref = 0
        do g = 1, k/8
            ref = ref + s(g, j)*sum(w((g - 1)*8 + 1:g*8, j)*x((g - 1)*8 + 1:g*8, m))
        end do
        if (y(j, m) /= ref) error stop "vector"
        if (z(j, m) /= ref) error stop "matrix"
    end do
end do
print *, "ok"
end program
