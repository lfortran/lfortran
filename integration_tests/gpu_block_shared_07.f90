module gpu_block_shared_07_m
implicit none
integer, parameter :: m = 48, n = 40
contains

! Phases of fewer iterations than the block has lanes, whose lanes are not
! a whole number of parts of the iteration (gpu_vector_lanes.cpp): 48 rows
! on 5 lanes each, and 12 on 21, the other lanes of the block idle in the
! shared-out loops and combining with the others through shared memory; a
! shared-out loop inside a loop of the row (`q`), which every lane goes
! through; an array of 12 elements shared by the block with one of 64;
! and the sums of the weights of a chunk of the rows, over a number of
! chunks known only when the kernel runs, read across the columns of `a`.
subroutine rows(a, nc, s, t, r)
    real, intent(in) :: a(:, :, :)
    integer, intent(in) :: nc
    real, intent(out) :: s(:, :), t(:, :, :), r(:, :)
    integer :: h
    do concurrent (h = 1:size(a, 3))
        block
            real :: big(64), small(12)
            real :: s1, s2, z
            integer :: i, j, q
            do concurrent (j = 1:64)
                big(j) = j + h
                big(j) = big(j) - mod(j, 3)
            end do
            do concurrent (i = 1:m)
                s1 = 0
                do j = 1, n
                    s1 = s1 + a(i, j, h)*big(j)
                end do
                do q = 1, 3
                    s2 = 0
                    do j = 1, 30
                        s2 = s2 + a(i, j, h)*q
                    end do
                    t(i, q, h) = s2 + s1
                end do
                s(i, h) = s1
            end do
            do concurrent (i = 1:12)
                z = 0
                do j = 1, n
                    z = max(z, a(i, j, h) + big(i))
                end do
                small(i) = z
            end do
            do concurrent (i = 1:12)
                z = 0
                do j = 1, nc
                    z = z + a(i, j, h)
                end do
                r(i, h) = small(i) + z
            end do
        end block
    end do
end subroutine

end module

program gpu_block_shared_07
use gpu_block_shared_07_m
implicit none
integer, parameter :: nh = 5
real :: a(m, n, nh), s(m, nh), t(m, 3, nh), r(12, nh), big(64), s1, s2, z
integer :: h, i, j, q

do h = 1, nh
    do j = 1, n
        do i = 1, m
            a(i, j, h) = mod(i + 3*j + 5*h, 9) - 4
        end do
    end do
end do
call rows(a, 17, s, t, r)
do h = 1, nh
    do j = 1, 64
        big(j) = j + h - mod(j, 3)
    end do
    do i = 1, m
        s1 = sum(a(i, :, h)*big(1:n))
        if (s(i, h) /= s1) then
            print *, "s", i, h, s(i, h), s1
            error stop 1
        end if
        do q = 1, 3
            s2 = sum(a(i, 1:30, h))*q
            if (t(i, q, h) /= s2 + s1) then
                print *, "t", i, q, h, t(i, q, h), s2 + s1
                error stop 2
            end if
        end do
    end do
    do i = 1, 12
        z = max(0.0, maxval(a(i, :, h) + big(i))) + sum(a(i, 1:17, h))
        if (r(i, h) /= z) then
            print *, "r", i, h, r(i, h), z
            error stop 3
        end if
    end do
end do
print *, "ok"
end program
